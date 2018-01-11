-- Jack Parser, as the coursework of Project 10 of Nand2Tetris course.
-- Author: Yuchen Pei
{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Functor.Identity
import Data.Either

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
--data JExpression = JExpression JTerm [(JBOp, JTerm)] deriving (Show, Eq)     -- JBOp can only be one of +-*/&|<>=
data JExpression = JIntConst Int 
           | JStrConst [Char]
           | JKeywordConst [Char]       -- can only be true, false, null or this
           | JExpVar JVarId
           | JExpCall JSubroutineCall 
           | JExpUna JUOp JExpression     -- JOp can only be - or ~ here
           | JExpBin JExpression [(JBOp, JExpression)]
           -- | JTermVarId JLeftVarId
           -- | JTermCall JSubroutineCall 
           -- | JTermExp JExpression 
               deriving (Show, Eq)
data JVarId = JVarId JIdentifier (Maybe JExpression) deriving (Show, Eq)
data JSubroutineCall = JSubroutineCall JIdentifier (Maybe JIdentifier) [JExpression] deriving (Show, Eq)
type JBOp = Char
type JUOp = Char
type JIdentifier = [Char]
type JTypeAndId = (JType, JIdentifier)
--type JParameter = JTypeAndId
--type JVarDec = JTypeAndId
type JClassVarScope = [Char]
type JType = [Char]
type JSubroutineType = [Char]
type JackParser = Parsec [Char] ()


binaryOpChars = "+-*/&|<>="
unaryOpChars = "-~"
keywordConstStrs = ["true", "false", "null", "this"]
primTypeStrs = ["int", "char", "boolean"]
primTypeStrs' = primTypeStrs ++ ["void"]
classVarScopeStrs = ["static", "field"]
subroutineTypeStrs = ["constructor", "function", "method"]
alphaUnderscore = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
alphaNumUnderscore = alphaUnderscore ++ ['0'..'9']

--str2JType xs = case xs of "int" -> JInt; "char" -> JChar; "boolean" -> JBoolean; "void" -> JVoid;
--str2JClassVarScope xs = case xs of "static" -> JStatic; "field" -> JField;
--str2JSubroutineType xs = case xs of "constructor" -> JConstructor; "function" -> JFunction; "method" -> JMethod;

parse' parser = parse parser ""

jack = parse' jClass

jClass :: JackParser JClass
jClass = do
  string "class" >> many1 space
  id <- jIdentifier
  many space >> char '{'
  --classVarDecs <- sepBy jClassVarDecs (many space)
  classVarDecs <- many $ try (many space >> jClassVarDecs)
  --subroutineDecs <- sepBy jSubroutineDec (many space)
  subroutineDecs <- many $ try (many space >> jSubroutineDec)
  many space >> char '}'
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
  many1 space
  typeAndIds <- jTypeAndIds
  many space >> char ';'
  return $ JClassVarDec scope <$> typeAndIds

jVarDecs :: JackParser [JTypeAndId]
jVarDecs = 
  (string "var" >> many1 space) >> jTypeAndIds <* (many space >> char ';')

jTypeAndId :: JackParser JType -> JackParser JTypeAndId
jTypeAndId p = do
  type_ <- p
  many1 space
  id <- jIdentifier
  return (type_, id)

jTypeAndIds :: JackParser [JTypeAndId]
jTypeAndIds = do
  type_ <- jType
  many1 space
  ids <- sepBy jIdentifier (many space >> char ',' >> many space)
  return $ (\x -> (type_, x)) <$> ids

jParameters :: JackParser [JTypeAndId]
jParameters = do 
  char '(' >> many space
  params <- sepBy (jTypeAndId jType) (try $ many space >> char ',' >> many space)
  many space >> char ')'
  return params

jSubroutineHeader :: JackParser JSubroutineHeader
jSubroutineHeader = do
  subtype <- jSubroutineType
  many1 space
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
jKeywordConst = JKeywordConst <$> (choice $ string <$> keywordConstStrs)

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
  many space
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
  op <- many space >> jBOp
  x <- many space >> jTerm
  return (op, x)

jSubroutineDec :: JackParser JSubroutineDec
jSubroutineDec = do
  header <- jSubroutineHeader
  many space
  body <- jSubroutineBody
  return $ JSubroutineDec header body

jSubroutineBody :: JackParser JSubroutineBody
jSubroutineBody = do
  char '{'
  varDecs <- many $ try (many space >> jVarDecs)
  stmts <- many $ try (many space >> jStatement)
  many space >> char '}'
  return $ JSubroutineBody (mconcat varDecs) stmts

jStatement :: JackParser JStatement
jStatement = choice [jLetStatement, jIfStatement, jWhileStatement, jDoStatement, jReturnStatement]

jLetStatement :: JackParser JStatement
jLetStatement = do
  string "let" >> many1 space
  leftVarId <- jVarId
  many space >> char '=' >> many space
  exp <- jExpression
  many space >> char ';'
  return $ JLetStatment leftVarId exp

jIfStatement = do
  string "if" >> many space >> char '(' >> many space
  exp <- jExpression
  many space >> char ')' >> many space >> char '{' >> many space
  stmts <- many jStatement
  many space >> char '}'
  stmts' <- optionMaybe $ try jElseBlock 
  return $ JIfStatement exp stmts stmts'

jElseBlock :: JackParser [JStatement]
jElseBlock = do
  many space >> string "else" >> many space >> char '{' >> many space
  stmts <- many jStatement
  many space >> char '}'
  return stmts

jWhileStatement :: JackParser JStatement
jWhileStatement = do
  string "while" >> many space >> char '(' >> many space
  exp <- jExpression
  many space >> char ')' >> many space >> char '{' >> many space
  stmts <- many jStatement
  return $ JWhileStatment exp stmts

jDoStatement :: JackParser JStatement
jDoStatement = do
  jCall <- string "do" >> many space >> jSubroutineCall 
  many space >> char ';'
  return $ JDoStatement jCall

jReturnStatement :: JackParser JStatement
jReturnStatement = do
  string "return" >> many space
  res <- optionMaybe $ try jExpression
  many space >> char ';'
  return $ JReturnStatement res

jSubroutineCall :: JackParser JSubroutineCall
jSubroutineCall = try $ do
  callee <- jIdentifier
  method <- optionMaybe $ try (char '.' >> jIdentifier)
  args <- emptyArgs <|> someArgs
  return $ JSubroutineCall callee method args

emptyArgs :: JackParser [JExpression]
emptyArgs = return [] <* (try $ char '(' >> many space >> char ')')

someArgs :: JackParser [JExpression]
someArgs = do
  char '(' >> many space
  exps <- sepBy jExpression (many space >> char ',' >> many space)
  many space >> char ')'
  return exps
