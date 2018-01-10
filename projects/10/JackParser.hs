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

jack = parse' jClass

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

jTypeAndId :: JackParser JTypeAndId
jTypeAndId = do
  type_ <- jType'
  many1 space
  id <- jIdentifier
  return (type_, id)

jTypeAndIds :: JackParser [JTypeAndId]
jTypeAndIds = do
  type_ <- jType'
  many1 space
  ids <- sepBy jIdentifier (many space >> char ',' >> many space)
  return $ (\x -> (type_, x)) <$> ids

jParameters :: JackParser [JTypeAndId]
jParameters = do 
  char '(' >> many space
  params <- sepBy jTypeAndId (many space >> char ',' >> many space)
  many space >> char ')'
  return params

jSubroutineHeader :: JackParser JSubroutineHeader
jSubroutineHeader = do
  subtype <- jSubroutineType
  many1 space
  typeAndId <- jTypeAndId
  params <- jParameters
  return $ JSubroutineHeader subtype typeAndId params

jTerm :: JackParser JTerm
jTerm = choice [jIntConst, jStrConst, jKeywordConst, jTermVarId, jTermCall, jTermExp, jUnaryOpTerm, jTermInBraces]

jIntConst :: JackParser JTerm
jIntConst = JIntConst <$> (read <$> many1 digit)

jStrConst :: JackParser JTerm
jStrConst = JStrConst <$> between (char '"') (char '"') (many $ noneOf "\"")

jKeywordConst :: JackParser JTerm
jKeywordConst = JKeywordConst <$> (choice $ string <$> keywordConstStrs)

jTermVarId :: JackParser JTerm
jTermVarId = JTermVarId <$> jLeftVarId

jLeftVarId :: JackParser JLeftVarId
jLeftVarId = do
  id <- jIdentifier
  maybeArray <- optionMaybe $ try (char '[' >> jExpression <* char ']')
  return $ JLeftVarId id maybeArray

jTermCall :: JackParser JTerm
jTermCall = JTermCall <$> jSubroutineCall

jTermExp :: JackParser JTerm
jTermExp = JTermExp <$> jExpression

jUnaryOpTerm :: JackParser JTerm
jUnaryOpTerm = do 
  op <- oneOf unaryOpChars
  many space
  term <- jTerm
  return $ JUnaryOpTerm op term

jTermInBraces :: JackParser JTerm
jTermInBraces = between (char '(') (char ')') jTerm

jExpression :: JackParser JExpression
jExpression = do
  x <- jTerm
  xs <- many jOpAndTerm
  return $ JExpression x xs

jOpAndTerm :: JackParser (JBOp, JTerm)
jOpAndTerm = do
  op <- many space >> jBOp
  term <- many space >> jTerm
  return (op, term)

jClass :: JackParser JClass
jClass = do
  string "class" >> many space >> char '{' >> many space
  id <- jIdentifier
  many space
  classVarDecs <- sepBy jClassVarDecs (many space)
  subroutineDecs <- sepBy jSubroutineDec (many space)
  many space >> char '}'
  return $ JClass id (mconcat classVarDecs) subroutineDecs

jSubroutineDec :: JackParser JSubroutineDec
jSubroutineDec = do
  header <- jSubroutineHeader
  many space
  body <- jSubroutineBody
  return $ JSubroutineDec header body

jSubroutineBody :: JackParser JSubroutineBody
jSubroutineBody = do
  char '{' >> many space
  varDecs <- sepBy jVarDecs (many space)
  stmts <- sepBy jStatement (many space)
  many space >> char '}'
  return $ JSubroutineBody (mconcat varDecs) stmts

jStatement :: JackParser JStatement
jStatement = choice [jLetStatement, jIfStatement, jWhileStatement, jDoStatement, jReturnStatement]

jLetStatement :: JackParser JStatement
jLetStatement = do
  string "let" >> many1 space
  leftVarId <- jLeftVarId
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
jSubroutineCall = do
  callee <- jIdentifier
  method <- optionMaybe $ try (char '.' >> jIdentifier)
  char '(' >> many space
  exps <- sepBy jExpression (many space >> char ',' >> many space)
  many space >> char ')'
  return $ JSubroutineCall callee method exps
