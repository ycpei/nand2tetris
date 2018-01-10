-- Jack Parser, as the coursework of Project 10 of Nand2Tetris course.
-- Author: Yuchen Pei
{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Functor.Identity
import Data.Either

--turingParse :: Stream s Identity Char => Parsec s () (Int, Char, Rule)
--solve1 xs = let (initN, initSt, rule) = fromRight (0, 'A', Map.empty) $ parse turingParse "" xs in
  --sum $ run initN 0 initSt [0] rule

data JClass = JClass JIdentifier [JClassVarDec] [JSubroutineDec] deriving (Show, Eq)
data JClassVarDec = JClassVarDec JClassVarScope JTypeAndId deriving (Show, Eq)
data JSubroutineDec = JSubroutineDec JSubroutineHeader JSubroutineBody deriving (Show, Eq)
data JSubroutineHeader = JSubroutineHeader JSubroutineType JTypeAndId [JParameter] deriving (Show, Eq)
data JSubroutineBody = JSubroutineBody [JVarDec] [JStatement] deriving (Show, Eq)
data JClassVarScope = JStatic | JField deriving (Show, Eq)
data JType = JInt | JChar | JBoolean | JVoid deriving (Show, Eq)
data JSubroutineType = JConstructor | JFunction | JMethod deriving (Show, Eq)
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
type JackParser = Parsec [Char] ()
type JTypeAndId = (JType, JIdentifier)
type JParameter = JTypeAndId
type JVarDec = JTypeAndId


binaryOpChars = "+-*/&|<>="
unaryOpChars = "-~"
keywordConstStrs = ["true", "false", "null", "this"]
typeStrs' = ["int", "char", "boolean"]
typeStrs = typeStrs' ++ ["void"]
classVarScopeStrs = ["static", "field"]
subroutineTypeStrs = ["constructor", "function", "method"]
alphaUnderscore = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']
alphaNumUnderscore = alphaUnderscore ++ ['0'..'9']

str2JType xs = case xs of "int" -> JInt; "char" -> JChar; "boolean" -> JBoolean; "void" -> JVoid;
str2JClassVarScope xs = case xs of "static" -> JStatic; "field" -> JField;
str2JSubroutineType xs = case xs of "constructor" -> JConstructor; "function" -> JFunction; "method" -> JMethod;

parse' parser = parse parser ""

skipSpaces = space >> skipMany space

jack = jClass

jClass :: JackParser JClass
jClass = undefined

jBOp :: JackParser JBOp
jBOp = oneOf binaryOpChars

jUOp :: JackParser JUOp
jUOp = oneOf unaryOpChars

jType :: JackParser JType
jType = fmap str2JType $ choice $ fmap string typeStrs

jType' :: JackParser JType
jType' = fmap str2JType $ choice $ fmap string typeStrs'

jClassVarScope :: JackParser JClassVarScope
jClassVarScope = fmap str2JClassVarScope $ choice $ fmap string classVarScopeStrs

jIdentifier :: JackParser [Char]
jIdentifier = do
  x <- oneOf alphaUnderscore
  xs <- many $ oneOf alphaNumUnderscore
  return $ x:xs

jClassVarDec :: JackParser JClassVarDec
jClassVarDec = do
  scope <- jClassVarScope
  skipSpaces
  typeAndId <- jTypeAndId
  many space >> char ';'
  return $ JClassVarDec scope typeAndId

jTypeAndId :: JackParser JTypeAndId
jTypeAndId = do
  type_ <- jType'
  skipSpaces
  id <- jIdentifier
  return (type_, id)
jParameter = many space >> jTypeAndId <* many space

jSubroutineType :: JackParser JSubroutineType
jSubroutineType = fmap str2JSubroutineType $ choice $ fmap string subroutineTypeStrs

{--
jSubroutineHeader :: JackParser JSubroutineHeader
jSubroutineHeader = do
  subtype <- jSubroutineType
  skipSpaces
  typeAndId <- jTypeAndId
  char '('
  params <- sepBy jParameter (char ',')
  char ')'
--}
