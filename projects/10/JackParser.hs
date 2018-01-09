-- Jack Parser, as the coursework of Project 10 of Nand2Tetris course.
-- Author: Yuchen Pei
data JClass = JClass JIdentifier [JClassVarDec] [JSubroutineDec]
data JIdentifier = JIdentifier [Char]
data JClassVarDec = JClassVarDec JClassVarScope JType JIdentifier
data JClassVarScope = JStatic | JField
data JType = JInt | JChar | JBoolean | JVoid
data JSubroutineDec = JSubroutineDec JSubroutineType JType JIdentifier [JParameter] JSubroutineBody
data JSubroutineType = JConstructor | JFunction | JMethod
data JParameter = JParameter JType JIdentifier
data JSubroutineBody = JSubroutineBody [JVarDec] [JStatement]
data JVarDec = JVarDec JType JIdentifier
data JStatement = JLetStatement | JIfStatement | JWhileStatment | JDoStatement | JReturnStatement
data JLetStatement = JLetStatment JLeftVarId JExpression
data JIfStatement = JIfStatement JExpression [JStatement] (Maybe [JStatement])
data JWhileStatment = JWhileStatment JExpression [JStatement]
type JDoStatement = JSubroutineCall
type JReturnStatement = Maybe JExpression
data JExpression = JTerm [(JOp, JTerm)]
data JTerm = JIntConst | JStrConst | JKeywordConst | JLeftVarId | JSubroutineCall | JExpression | JUnaryOpTerm
data JLeftVarId = JIdentifier | JLeftVarId JIdentifier (Maybe JExpression)
data JSubroutineCall = JSubroutineCall Identifier [JExpression] | JSubroutineCall Identifier Identifier [JExpression]
type JOp = Char
data JUnaryOpTerm = JUnaryOpTerm JUnaryOp Term
type JUnaryOp = Char
data JKeywordConst = JTrue | JFalse | JNull | JThis
