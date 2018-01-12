-- Jack Compiler, as the coursework of Project 11 of Nand2Tetris course.
-- Author: Yuchen Pei (me@ypei.me)
-- Date: 2018-01-12
{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Functor.Identity
import Data.Either
import Data.Maybe
import Data.List
import System.Environment
import System.Directory
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

data JClass = JClass JIdentifier [JClassVarDec] [JSubroutineDec] deriving (Show, Eq)
data JClassVarDec = JClassVarDec JClassVarScope JTypeAndId deriving (Show, Eq)
data JSubroutineDec = JSubroutineDec JSubroutineHeader JSubroutineBody deriving (Show, Eq)
data JSubroutineHeader = JSubroutineHeader JSubroutineType JTypeAndId [JTypeAndId] deriving (Show, Eq)
data JSubroutineBody = JSubroutineBody [JTypeAndId] [JStatement] deriving (Show, Eq)
data JStatement = JLetStatment JVarId JExpression 
                | JIfStatement JExpression [JStatement] (Maybe [JStatement])
                | JWhileStatment JExpression [JStatement]
                | JDoStatement JSubroutineCall
                | JReturnStatement (Maybe JExpression) 
                    deriving (Show, Eq)
data JExpression = JIntConst Int 
           | JStrConst [Char]
           | JKeywordConst [Char]       -- can only be true, false, null or this
           | JExpVar JVarId
           | JExpCall JSubroutineCall 
           | JExpUna JUOp JExpression     -- JOp can only be - or ~ here
           | JExpBin JExpression [(JBOp, JExpression)]
               deriving (Show, Eq)
data JVarId = JVarId JIdentifier (Maybe JExpression) deriving (Show, Eq)
data JSubroutineCall = JSubroutineCall JIdentifier (Maybe JIdentifier) [JExpression] deriving (Show, Eq)

data ClassTable = C [Char] Table Int deriving (Show, Eq)

type JBOp = Char
type JUOp = Char
type JIdentifier = [Char]
type JTypeAndId = (JType, JIdentifier)
type JClassVarScope = [Char]
type JType = [Char]
type JSubroutineType = [Char]
type JackParser = Parsec [Char] ()
type Table = Map [Char] ([Char], [Char], Int)


binaryOpChars = "+-*/&|<>="
unaryOpChars = "-~"
keywordConstStrs = ["true", "false", "null", "this"]
primTypeStrs = ["int", "char", "boolean"]
primTypeStrs' = primTypeStrs ++ ["void"]
classVarScopeStrs = ["static", "field"]
subroutineTypeStrs = ["constructor", "function", "method"]
alphaUnderscore = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
alphaNumUnderscore = alphaUnderscore ++ ['0'..'9']

parse' parser = parse parser ""

--jack xs = parse' (many jSpace >> jClass) (replCrWithNl xs)
jack = parse' (many jSpace >> jClass)

jClass :: JackParser JClass
jClass = do
  string "class" >> many1 jSpace
  id <- jIdentifier
  many jSpace >> char '{'
  classVarDecs <- many $ try (many jSpace >> jClassVarDecs)
  subroutineDecs <- many $ try (many jSpace >> jSubroutineDec)
  many jSpace >> char '}'
  return $ JClass id (mconcat classVarDecs) subroutineDecs

jClassType :: JackParser [Char]
jClassType = do 
  x <- oneOf ['A' .. 'Z']
  xs <- many $ oneOf alphaNumUnderscore
  return $ x:xs

jBOp :: JackParser JBOp
jBOp = oneOf binaryOpChars

jUOp :: JackParser JUOp
jUOp = oneOf unaryOpChars

jComment :: JackParser ()
jComment = jInlineComment <|> jBlockComment

jInlineComment :: JackParser ()
jInlineComment = return () <*
  try (string "//" >> manyTill (noneOf "\n\r") endOfLine)
  --try (string "//" >> manyTill (noneOf "\n") newline)

jSpace :: JackParser ()
jSpace = (return () <* space) <|> jComment

jBlockComment :: JackParser ()
jBlockComment = return () <*
  try (string "/*" >> manyTill anyChar (try $ string "*/"))

jPrimType :: JackParser JType
jPrimType = choice $ string <$> primTypeStrs

jType :: JackParser JType
jType = choice [jPrimType, jClassType]

jPrimType' :: JackParser JType
jPrimType' = choice $ string <$> primTypeStrs'

jType' :: JackParser JType
jType' = choice [jPrimType', jClassType]

jClassVarScope :: JackParser JClassVarScope
jClassVarScope = choice $ string <$> classVarScopeStrs

jSubroutineType :: JackParser JSubroutineType
jSubroutineType = choice $ string <$> subroutineTypeStrs

jIdentifier :: JackParser [Char]
jIdentifier = do
  x <- oneOf alphaUnderscore
  xs <- many $ oneOf alphaNumUnderscore
  return $ x:xs

jClassVarDecs :: JackParser [JClassVarDec]
jClassVarDecs = do
  scope <- jClassVarScope
  many1 jSpace
  typeAndIds <- jTypeAndIds
  many jSpace >> char ';'
  return $ JClassVarDec scope <$> typeAndIds

jVarDecs :: JackParser [JTypeAndId]
jVarDecs = 
  (string "var" >> many1 jSpace) >> jTypeAndIds <* (many jSpace >> char ';')

jTypeAndId :: JackParser JType -> JackParser JTypeAndId
jTypeAndId p = do
  type_ <- p
  many1 jSpace
  id <- jIdentifier
  return (type_, id)

jTypeAndIds :: JackParser [JTypeAndId]
jTypeAndIds = do
  type_ <- jType
  many1 jSpace
  ids <- sepBy jIdentifier (try $ many jSpace >> char ',' >> many jSpace)
  return $ (\x -> (type_, x)) <$> ids

jParameters :: JackParser [JTypeAndId]
jParameters = do 
  char '(' >> many jSpace
  params <- sepBy (jTypeAndId jType) (try $ many jSpace >> char ',' >> many jSpace)
  many jSpace >> char ')'
  return params

jSubroutineHeader :: JackParser JSubroutineHeader
jSubroutineHeader = do
  subtype <- jSubroutineType
  many1 jSpace
  typeAndId <- jTypeAndId jType'
  params <- jParameters
  return $ JSubroutineHeader subtype typeAndId params

jExpression :: JackParser JExpression
jExpression = jExpBin <|> jTerm

jTerm :: JackParser JExpression
jTerm = choice [jIntConst, jStrConst, jKeywordConst, jExpCall, jExpVar, jExpUna, jExpInBraces]

jIntConst :: JackParser JExpression
jIntConst = JIntConst <$> (read <$> many1 digit)

jStrConst :: JackParser JExpression
jStrConst = JStrConst <$> between (char '"') (char '"') (many $ noneOf "\"")

jKeywordConst :: JackParser JExpression
jKeywordConst = JKeywordConst <$> (choice $ try <$> string <$> keywordConstStrs)

jExpVar :: JackParser JExpression
jExpVar = JExpVar <$> jVarId

jVarId :: JackParser JVarId
jVarId = do
  id <- jIdentifier
  maybeArray <- optionMaybe $ try (char '[' >> jExpression <* char ']')
  return $ JVarId id maybeArray

jExpCall :: JackParser JExpression
jExpCall = JExpCall <$> jSubroutineCall

jExpUna :: JackParser JExpression
jExpUna = do 
  op <- oneOf unaryOpChars
  many jSpace
  x <- jTerm
  return $ JExpUna op x

jExpInBraces :: JackParser JExpression
jExpInBraces = between (char '(') (char ')') jExpression  -- expressions like a + () is not allowed

jExpBin :: JackParser JExpression
jExpBin = try $ do
  x <- jTerm
  xs <- many1 jOpAndTerm
  return $ JExpBin x xs

jOpAndTerm :: JackParser (JBOp, JExpression)
jOpAndTerm = do
  op <- many jSpace >> jBOp
  x <- many jSpace >> jTerm
  return (op, x)

jSubroutineDec :: JackParser JSubroutineDec
jSubroutineDec = do
  header <- jSubroutineHeader
  many jSpace
  body <- jSubroutineBody
  return $ JSubroutineDec header body

jSubroutineBody :: JackParser JSubroutineBody
jSubroutineBody = do
  char '{'
  varDecs <- many $ try (many jSpace >> jVarDecs)
  stmts <- many $ try (many jSpace >> jStatement)
  many jSpace >> char '}'
  return $ JSubroutineBody (mconcat varDecs) stmts

jStatement :: JackParser JStatement
jStatement = choice [jLetStatement, jIfStatement, jWhileStatement, jDoStatement, jReturnStatement]

jLetStatement :: JackParser JStatement
jLetStatement = do
  string "let" >> many1 jSpace
  leftVarId <- jVarId
  many jSpace >> char '=' >> many jSpace
  exp <- jExpression
  many jSpace >> char ';'
  return $ JLetStatment leftVarId exp

jIfStatement = do
  string "if" >> many jSpace >> char '(' >> many jSpace
  exp <- jExpression
  many jSpace >> char ')' >> many jSpace >> char '{' >> many jSpace
  stmts <- many (try $ many jSpace >> jStatement)
  many jSpace >> char '}'
  stmts' <- optionMaybe $ try jElseBlock 
  return $ JIfStatement exp stmts stmts'

jElseBlock :: JackParser [JStatement]
jElseBlock = do
  many jSpace >> string "else" >> many jSpace >> char '{' >> many jSpace
  stmts <- many (try $ many jSpace >> jStatement)
  many jSpace >> char '}'
  return stmts

jWhileStatement :: JackParser JStatement
jWhileStatement = do
  string "while" >> many jSpace >> char '(' >> many jSpace
  exp <- jExpression
  many jSpace >> char ')' >> many jSpace >> char '{'
  stmts <- many (try $ many jSpace >> jStatement)
  many jSpace >> char '}'
  return $ JWhileStatment exp stmts

jDoStatement :: JackParser JStatement
jDoStatement = do
  jCall <- string "do" >> many jSpace >> jSubroutineCall 
  many jSpace >> char ';'
  return $ JDoStatement jCall

jReturnStatement :: JackParser JStatement
jReturnStatement = do
  string "return" >> many jSpace
  res <- optionMaybe $ try jExpression
  many jSpace >> char ';'
  return $ JReturnStatement res

jSubroutineCall :: JackParser JSubroutineCall
jSubroutineCall = try $ do
  callee <- jIdentifier
  method <- optionMaybe $ try (char '.' >> jIdentifier)
  args <- emptyArgs <|> someArgs
  return $ JSubroutineCall callee method args

emptyArgs :: JackParser [JExpression]
emptyArgs = return [] <* (try $ char '(' >> many jSpace >> char ')')

someArgs :: JackParser [JExpression]
someArgs = do
  char '(' >> many jSpace
  exps <- sepBy jExpression (many jSpace >> char ',' >> many jSpace)
  many jSpace >> char ')'
  return exps

-- vm writer starts from here

buildCTable :: [Char] -> [JClassVarDec] -> ClassTable
buildCTable name xs = go 0 0 Map.empty xs
  where go n _ t [] = C name t n
        go nField nStatic t ((JClassVarDec "field" (ty, jId)):ys) =
          go (nField + 1) nStatic (Map.insert jId (ty, "field", nField) t) ys
        go nField nStatic t ((JClassVarDec "static" (ty, jId)):ys) =
          go nField (nStatic + 1) (Map.insert jId (ty, "static", nStatic) t) ys
          

--data JClass = JClass JIdentifier [JClassVarDec] [JSubroutineDec] deriving (Show, Eq)
buildSRTable :: [JClass] -> Table
buildSRTable xs = Map.fromList $ mconcat $ go <$> xs where 
  go (JClass cName _ subs) = go' <$> subs where
    go' (JSubroutineDec (JSubroutineHeader kind (ty, sName) args) _) = 
      (cName ++ "." ++ sName, (ty, kind, length args))

buildLTable :: [JTypeAndId] -> [JTypeAndId] -> Table
buildLTable args lcls = 
  (go "argument" args 0 Map.empty) `Map.union` (go "local" lcls 0 Map.empty) where
    go _ [] _ t = t
    go kind ((ty, name):xs) n t = go kind xs (n + 1) $ Map.insert name (ty, kind, n) t
       
--data JSubroutineDec = JSubroutineDec JSubroutineHeader JSubroutineBody deriving (Show, Eq)
--data JSubroutineHeader = JSubroutineHeader JSubroutineType JTypeAndId [JTypeAndId] deriving (Show, Eq)
--data JSubroutineBody = JSubroutineBody [JTypeAndId] [JStatement] deriving (Show, Eq)

vSubroutine :: ClassTable -> JSubroutineDec -> [Char]
vSubroutine cTable sub = 
  vFunction (cName ++ "." ++ sName) nLcls ++ 
  (if sType == "constructor" vNew n else "") ++
  mconcat $ vStmts where 
    C cName t n = cTable
    JSubroutineDec (JSubroutineHeader _ (_, sName) args) (JSubroutineBody lcls stmts) = sub
    nLcls = length lcls
    s = buildLTable args lcls
    vStmts = vStatement t s sName 0 stmts

-- data JStatement = JLetStatment JVarId JExpression 
                -- | JIfStatement JExpression [JStatement] (Maybe [JStatement])
                -- | JWhileStatment JExpression [JStatement]
                -- | JDoStatement JSubroutineCall
                -- | JReturnStatement (Maybe JExpression) 

vStatement _ _ _ _ [] = ""

vStatement t s name n ((JLetStatment var exp):stmts) =
-- vExpression: push the result of exp; vPopToVar: pop to the var addr
  vExpression t s exp ++ vPopToVar t s var ++ vStatement t s name n stmts 

vStatement t s name n ((JIfStatement cond thenStmts elseStmts):stmts) =
  vExpression t s cond ++ vNot ++ vThen thenStmts ++ 
  maybe (vLabel labelElse) vElse elseStmts ++ vStatement t s name (n + 1) stmts where 
    labelElse = name ++ ".Else" ++ show n
    labelEndIf = name ++ ".Endif" ++ show n
    vThen xs = vIfGoto labelElse ++ vStatement t s labelElse 0 xs
    vElse xs = vGoto labelEndIf ++ vLabel labelElse ++ 
               vStatement t s labelIf 0 xs ++ vLabel labelEndIf

vStatement t s name n ((JWhileStatment cond loopStmts):stmts) = 
  vLabel labelWhile ++ vExpression t s cond ++ vNot ++ vIfGoto labelEndWhile ++ 
  vStatement t s labelWhile 0 loopStmts ++ vGoto labelWhile ++ 
  vLabel labelEndWhile ++ vStatement t s name (n + 1) stmts where
    labelWhile = name ++ ".While" ++ show n
    labelEndWhile = name ++ ".EndWhile" ++ show n

vStatement t s name n ((JDoStatement subCall):stmts) =
  vSubroutineCall subCall ++ vPop "temp" 0 ++ vStatement t s name n stmts

vStatement t s name n ((JReturnStatement ret):stmts) =
  maybe (vPush "constant" 0) (vExpression t s) ret ++ vReturn ++
  vStatement t s name n stmts

-- data JExpression = JIntConst Int 
           -- | JStrConst [Char]
           -- | JKeywordConst [Char]       -- can only be true, false, null or this
           -- | JExpVar JVarId
           -- | JExpCall JSubroutineCall 
           -- | JExpUna JUOp JExpression     -- JOp can only be - or ~ here
           -- | JExpBin JExpression [(JBOp, JExpression)]

vExpression t s (JIntConst n) = push "constant" n
vExpression t s (JStrConst xs) = 



vNew n = vPush "constant" n ++ vCall "Memory.alloc" 1 ++ vPop "pointer" 0
vNot = "not\n"
vReturn = "return\n"
vLabel xs = "label " ++ xs ++ "\n"
vGoto xs = "goto " ++ xs ++ "\n"
vIfGoto xs = "if-goto " ++ xs ++ "\n"
vFunction xs n = "function " ++ xs ++ " " ++ show n ++ "\n"
vCall xs n = "call " ++ xs ++ " " ++ show n ++ "\n"
vPush xs n = "push " ++ xs ++ " " ++ show n ++ "\n"
vPop xs n = "pop " ++ xs ++ " " ++ show n ++ "\n"

--vClass :: JClass -> [Char]
--vClass (JClass cName cVars subs) =
  --vSubroutine (table cName cVars) <$> subs


fst3 (x, y, z) = x
snd3 (x, y, z) = y
trd3 (x, y, z) = z

{--
replCrWithNl :: [Char] -> [Char]
replCrWithNl = fmap cr2nl 
  where cr2nl '\r' = '\n'
        cr2nl c = c
        --}

-- IO

{--
main = do
  dir <- head <$> getArgs
  filesWODir <- filter isJackFile <$> listDirectory dir 
  let jackFiles = (dir++) <$> filesWODir
  codes <- sequence $ readFile <$> jackFiles
  zipWithM writeFile (chExt <$> jackFiles) (show . jack <$> codes)
    where isJackFile xs = drop (length xs - 5) xs == ".jack"
          chExt xs = take (length xs - 4) xs ++ "ast"
          --}
