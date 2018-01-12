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
import Data.Char
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

--data ClassTable = C [Char] Table Int deriving (Show, Eq)
type JBOp = Char
type JUOp = Char
type JIdentifier = [Char]
type JTypeAndId = (JType, JIdentifier)
type JClassVarScope = [Char]
type JType = [Char]
type JSubroutineType = [Char]
type JackParser = Parsec [Char] ()
-- For variable tables: map name (type, (seg, n))
-- For the subroutine table: map name (kind, (type, nArgs))
type Table = Map [Char] ([Char], ([Char], Int))


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
jackReader = parse' (many jSpace >> jClass)

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

buildCTable :: [JClassVarDec] -> (Table, Int)
buildCTable xs = go 0 0 Map.empty xs
  where go n _ t [] = (t, n)
        go nField nStatic t ((JClassVarDec "field" (ty, jId)):ys) =
          go (nField + 1) nStatic (Map.insert jId (ty, ("this", nField)) t) ys
        go nField nStatic t ((JClassVarDec "static" (ty, jId)):ys) =
          go nField (nStatic + 1) (Map.insert jId (ty, ("static", nStatic)) t) ys
          

--data JClass = JClass JIdentifier [JClassVarDec] [JSubroutineDec] deriving (Show, Eq)
buildSRTable :: [JClass] -> Table
buildSRTable xs = Map.fromList $ mconcat $ go <$> xs where 
  go (JClass cName _ subs) = go' <$> subs where
    go' (JSubroutineDec (JSubroutineHeader kind (ty, sName) args) _) = 
      (cName ++ "." ++ sName, (kind, (ty, nArgs))) where
        nArgs = length args + if kind == "method" then 1 else 0

buildLTable :: [JTypeAndId] -> [JTypeAndId] -> Table
buildLTable args lcls = 
  (go "argument" args 0 Map.empty) `Map.union` (go "local" lcls 0 Map.empty) where
    go _ [] _ t = t
    go kind ((ty, name):xs) n t = go kind xs (n + 1) $ Map.insert name (ty, (kind, n)) t
       
--data JSubroutineDec = JSubroutineDec JSubroutineHeader JSubroutineBody deriving (Show, Eq)
--data JSubroutineHeader = JSubroutineHeader JSubroutineType JTypeAndId [JTypeAndId] deriving (Show, Eq)
--data JSubroutineBody = JSubroutineBody [JTypeAndId] [JStatement] deriving (Show, Eq)

jackCompiler :: [[Char]] -> [[Char]]
jackCompiler = vmWriter . fmap (fromRight (JClass "" [] []) . jackReader)

vmWriter :: [JClass] -> [[Char]]
vmWriter xs = vClass (buildSRTable xs) <$> xs

vClass :: Table -> JClass -> [Char]
vClass u (JClass name vars subs) = mconcat $ vSubroutineDec name t u n <$> subs where
  (t, n) = buildCTable vars

vSubroutineDec :: [Char] -> Table -> Table -> Int -> JSubroutineDec -> [Char]
vSubroutineDec cName t u n sub = 
  vFunction (cName ++ "." ++ sName) nLcls ++ kindSpec ++
  vStatement t s u sName 0 stmts where 
    JSubroutineDec (JSubroutineHeader _ (_, sName) args) (JSubroutineBody lcls stmts) = sub
    nLcls = length lcls
    s = buildLTable args lcls
    kind = fst $ u Map.! (cName ++ "." ++ sName)
    kindSpec = if kind == "constructor" 
      then vNew n 
      else if kind == "method"
        then vPush "argument" 0 ++ vPop "pointer" 0
        else ""

-- data JStatement = JLetStatment JVarId JExpression 
                -- | JIfStatement JExpression [JStatement] (Maybe [JStatement])
                -- | JWhileStatment JExpression [JStatement]
                -- | JDoStatement JSubroutineCall
                -- | JReturnStatement (Maybe JExpression) 

vStatement _ _ _ _ _ [] = ""

vStatement t s u name n ((JLetStatment var exp):stmts) =
-- vExpression: push the result of exp; vPopToVar: pop to the var addr
  vExpression t s u name exp ++ vPopToVar t s u name var ++ vStatement t s u name n stmts 

vStatement t s u name n ((JIfStatement cond thenStmts elseStmts):stmts) =
  vExpression t s u name cond ++ vNot ++ vThen thenStmts ++ 
  maybe (vLabel labelElse) vElse elseStmts ++ vStatement t s u name (n + 1) stmts where 
    labelElse = name ++ ".Else" ++ show n
    labelEndIf = name ++ ".Endif" ++ show n
    labelIf = name ++ ".If" ++ show n
    vThen xs = vIfGoto labelElse ++ vStatement t s u labelElse 0 xs
    vElse xs = vGoto labelEndIf ++ vLabel labelElse ++ 
               vStatement t s u labelIf 0 xs ++ vLabel labelEndIf

vStatement t s u name n ((JWhileStatment cond loopStmts):stmts) = 
  vLabel labelWhile ++ vExpression t s u name cond ++ vNot ++ vIfGoto labelEndWhile ++ 
  vStatement t s u labelWhile 0 loopStmts ++ vGoto labelWhile ++ 
  vLabel labelEndWhile ++ vStatement t s u name (n + 1) stmts where
    labelWhile = name ++ ".While" ++ show n
    labelEndWhile = name ++ ".EndWhile" ++ show n

vStatement t s u name n ((JDoStatement subCall):stmts) =
  vSubroutineCall t s u name subCall ++ vPop "temp" 0 ++ vStatement t s u name n stmts

vStatement t s u name n ((JReturnStatement ret):stmts) =
  maybe (vPush "constant" 0) (vExpression t s u name) ret ++ vReturn ++
  vStatement t s u name n stmts

-- data JExpression = JIntConst Int 
           -- | JStrConst [Char]
           -- | JKeywordConst [Char]       -- can only be true, false, null or this
           -- | JExpVar JVarId
           -- | JExpCall JSubroutineCall 
           -- | JExpUna JUOp JExpression     -- JOp can only be - or ~ here
           -- | JExpBin JExpression [(JBOp, JExpression)]

vExpression :: Table -> Table -> Table -> [Char] -> JExpression -> [Char]

vExpression t s _ _ (JIntConst n) = vPush "constant" n

vExpression t s _ _ (JStrConst xs) = 
  vPush "constant" (length xs) ++ vCall "String.new" 1 ++ mconcat (go <$> xs) where
    go x = vPush "constant" (ord x) ++ vCall "String.appendChar" 2

vExpression t s _ _ (JKeywordConst x) 
  | x == "null" || x == "false" = vPush "constant" 0
  | x == "true" = vPush "constant" 1 ++ vNeg
  | x == "this" = vPush "pointer" 0

vExpression t s u cName (JExpVar (JVarId name idx)) = vVar t s u cName name idx vPush

vExpression t s u cName (JExpCall subCall) = vSubroutineCall t s u cName subCall

vExpression t s u cName (JExpUna op exp) = vExpression t s u cName exp ++ vOp op
  where vOp '~' = vNot; vOp '-' = vNeg;

vExpression t s u cName (JExpBin exp xs) = 
  vExpression t s u cName exp ++ mconcat (go <$> xs) 
    where go (op, exp) = vExpression t s u cName exp ++ vOp op
          vOp '+' = vAdd; vOp '-' = vSub; vOp '*' = vMul; vOp '/' = vDiv;
          vOp '&' = vAnd; vOp '|' = vOr;  vOp '<' = vLt;  vOp '>' = vGt;
          vOp '=' = vEq;

vPopToVar t s u cName (JVarId name idx) = vVar t s u cName name idx vPop

vVar :: Table -> Table -> Table -> [Char] -> [Char] -> Maybe JExpression -> ([Char] -> Int -> [Char]) -> [Char]
vVar t s u cName name idx vPushOrPop = 
  let (seg, n) = getSegN t s name in 
    case idx of
      Nothing -> vPushOrPop seg n
      Just exp -> vPush seg n ++ vExpression t s u cName exp ++ vAdd 
                       ++ vPop "pointer" 1 ++ vPushOrPop "that" 0
      

getRecord :: Table -> Table -> [Char] -> ([Char], ([Char], Int))
getRecord t s name = case Map.lookup name s of
  Just x -> x
  Nothing -> t Map.! name

getSegN :: Table -> Table -> [Char] -> ([Char], Int)
getSegN t s name = snd $ getRecord t s name

getType :: Table -> Table -> [Char] -> [Char]
getType t s name = fst $ getRecord t s name

-- data JSubroutineCall = JSubroutineCall JIdentifier (Maybe JIdentifier) [JExpression] deriving (Show, Eq)
vSubroutineCall t s u cName (JSubroutineCall name name' args) = 
  method ++ mconcat (vExpression t s u cName <$> args) ++ vCall name'' nArgs
    where 
      name'' = if name' == Nothing 
                 then cName ++ "." ++ name 
                 else if head name `elem` ['A' .. 'Z'] 
                        then name ++ "." ++ fromJust name'
                        else getType t s name ++ "." ++ fromJust name'
      -- u is the SRTable
      (method, nArgs) = 
         if fst (u Map.! name'') == "method"
           then let (seg, n) = getSegN t s name in (vPush seg n, length args + 1)
           else ("", length args)

vNew n = vPush "constant" n ++ vCall "Memory.alloc" 1 ++ vPop "pointer" 0
vAdd = "add\n"
vNot = "not\n"
vNeg = "neg\n"
vSub = "sub\n"
vMul = "call Math.multiply 2\n"
vDiv = "call Math.division 2\n"
vAnd = "and\n"
vOr = "or\n"
vLt = "lt\n"
vGt = "gt\n"
vEq = "eq\n"
vReturn = "return\n"
vLabel xs = "label " ++ xs ++ "\n"
vGoto xs = "goto " ++ xs ++ "\n"
vIfGoto xs = "if-goto " ++ xs ++ "\n"
vFunction xs n = "function " ++ xs ++ " " ++ show n ++ "\n"
vCall xs n = "call " ++ xs ++ " " ++ show n ++ "\n"
vPush xs n = "push " ++ xs ++ " " ++ show n ++ "\n"
vPop xs n = "pop " ++ xs ++ " " ++ show n ++ "\n"

inputSeven = ["// This file is part of www.nand2tetris.org\n// and the book \"The Elements of Computing Systems\"\n// by Nisan and Schocken, MIT Press.\n// File name: projects/11/Seven/Main.jack\n\n/**\n * Computes the value of 1 + (2 * 3) and prints the result\n * at the top-left of the screen.  \n */\nclass Main {\n\n   function void main() {\n      do Output.printInt(1 + (2 * 3));\n      return;\n   }\n}\n"]

inputSeven1 = "do Output.printInt(1 + (2 * 3));"

test reader writer x = let Right y = parse' reader x in writer y
test' x = let Right y = parse' jStatement x in vStatement Map.empty Map.empty Map.empty "" 0 [y]

{--
fst3 (x, y, z) = x
snd3 (x, y, z) = y
trd3 (x, y, z) = z
--}

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
