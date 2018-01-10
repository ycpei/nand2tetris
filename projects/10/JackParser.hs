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
data JStatement = JLetStatment JLeftVarId JExpression 
                | JIfStatement JExpression [JStatement] (Maybe [JStatement])
                | JWhileStatment JExpression [JStatement]
                | JDoStatement JSubroutineCall
                | JReturnStatement (Maybe JExpression) 
                    deriving (Show, Eq)
data JExpression = JExpression JTerm [(JBOp, JTerm)] deriving (Show, Eq)     -- JBOp can only be one of +-*/&|<>=
data JTerm = JIntConst Int 
           | JStrConst [Char]
           | JKeywordConst [Char]       -- can only be true, false, null or this
           | JTermVarId JLeftVarId
           | JTermCall JSubroutineCall 
           | JTermExp JExpression 
           | JUnaryOpTerm JUOp JTerm     -- JOp can only be - or ~ here
               deriving (Show, Eq)
data JLeftVarId = JLeftVarId JIdentifier (Maybe JExpression) deriving (Show, Eq)
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
typeStrs' = ["int", "char", "boolean"]
typeStrs = typeStrs' ++ ["void"]
classVarScopeStrs = ["static", "field"]
subroutineTypeStrs = ["constructor", "function", "method"]
alphaUnderscore = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
alphaNumUnderscore = alphaUnderscore ++ ['0'..'9']

--str2JType xs = case xs of "int" -> JInt; "char" -> JChar; "boolean" -> JBoolean; "void" -> JVoid;
--str2JClassVarScope xs = case xs of "static" -> JStatic; "field" -> JField;
--str2JSubroutineType xs = case xs of "constructor" -> JConstructor; "function" -> JFunction; "method" -> JMethod;

parse' parser = parse parser ""

jack = jClass

jClass :: JackParser JClass
jClass = undefined

jBOp :: JackParser JBOp
jBOp = oneOf binaryOpChars

jUOp :: JackParser JUOp
jUOp = oneOf unaryOpChars

jType :: JackParser JType
jType = choice $ string <$> typeStrs

jType' :: JackParser JType
jType' = choice $ string <$> typeStrs'

jClassVarScope :: JackParser JClassVarScope
jClassVarScope = choice $ string <$> classVarScopeStrs

jSubroutineType :: JackParser JSubroutineType
jSubroutineType = choice $ string <$> subroutineTypeStrs

jIdentifier :: JackParser [Char]
jIdentifier = do
  x <- oneOf alphaUnderscore
  xs <- many $ oneOf alphaNumUnderscore
  return $ x:xs

jClassVarDec :: JackParser JClassVarDec
jClassVarDec = do
  scope <- jClassVarScope
  many1 space
  typeAndId <- jVarDec
  many space >> char ';'
  return $ JClassVarDec scope typeAndId

jTypeAndId :: JackParser JTypeAndId
jTypeAndId = do
  type_ <- jType'
  many1 space
  id <- jIdentifier
  return (type_, id)

jParameter :: JackParser JParameter
jParameter = many space >> jTypeAndId <* many space

jSubroutineHeader :: JackParser JSubroutineHeader
jSubroutineHeader = do
  subtype <- jSubroutineType
  many1 space
  typeAndId <- jTypeAndId
  char '('
  params <- sepBy jParameter (char ',')
  char ')'
  return $ JSubroutineHeader subtype typeAndId params

jTerm :: JackParser JTerm
jTerm = choice [jIntConst, jStrConst, jKeywordConst, jLeftVarId, jTermCall, jTermExp, jUnaryOpTerm, jTermInBraces]

jIntConst :: JackParser JTerm
jIntConst = JIntConst <$> (read <$> many1 digit)

jStrConst :: JackParser JTerm
jStrConst = JStrConst <$> between (char '"') (char '"') (many $ noneOf "\"")

jKeywordConst :: JackParser JTerm
jKeywordConst = JKeywordConst <$> (choice $ string <$> keywordConstStrs)

jLeftVarId :: JackParser JTerm
jLeftVarId = do
  id <- jIdentifier
  maybeArray <- option Nothing $ try (char '[' >> jExpression <* char ']')
  return JLeftVarId id maybeArray

jTermCall :: JackParser JTerm
jTermCall = JTermCall <$> jSubroutineCall

jTermExp :: JackParser JTerm
jTermExp = JTermExp <$> jExpression

jUnaryOpTerm :: JackParser JTerm
jUnaryOpTerm = do 
  op <- oneOf unaryOpChars
  many space
  term <- JTerm
  return $ JUnaryOpTerm op term

jTermInBraces :: JackParser JTerm
jTermInBraces = between (char '(') (char ')') jTerm

jExpression :: JackParser JExpression
jExpression = do
  x <- jTerm
  xs <- many jOpAndTerm
  return $ JExpression x xs

jOpAndTerm :: JackParser (jBOp, jTerm)
jOpAndTerm = do
  op <- many space >> jBOp
  term <- many space >> jTerm
  return (op, term)

jClass :: JackParser JClass
jClass = do
  string "class" >> many space >> char '{' >> many space
  id <- jIdentifier
  many space
  classVarDecs <- sepBy jClassVarDec (many space)
  subroutineDecs <- sepBy jSubroutineDec (many space)
  many space >> char '}'
  return $ JClass id classVarDecs subroutineDecs

jSubroutineDec :: JackParser JSubroutineDec
jSubroutineDec = do
  header <- jSubroutineHeader
  many space
  body <- jSubroutineBody
  return JSubroutineDec header body

jSubroutineBody :: JackParser JSubroutineBody
jSubroutineBody = do
  char '{' >> many space
  varDecs <- sepBy jVarDec (many space)
  stmts <- sepBy jStatement (many space)
  many space >> char '}'
  return JSubroutineBody varDecs stmts
