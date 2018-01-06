--usage: ./VMTranslator path/to/dir/
--will produce path/to/dir/dir.asm from path/to/dir/*.vm
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import System.Directory (listDirectory)

--preamble = "@256\nD=A\n@SP\nM=D\n"
--For ProgramFlow and SimpleFunction preamble should be set as ""
--preamble = ""
--For StaticsTest and FibonacciElement, preamble should be setSP ++ callInit
preamble = setSP ++ callInit
  where setSP = "@256\nD=A\n@SP\nM=D\n"
        callInit = parse' "call" ["Sys.init", "0"] 0 "" ""

--epilogue = "(END)\n@END\n0;JMP"
epilogue = ""

-- parse' command restOfCommand vmLineNumber fileName functionName = asm code
parse' :: [Char] -> [[Char]] -> Int -> [Char] -> [Char] -> [Char]

parse' "return" [] _ _ _ = backupLCL ++ backupRet ++ popToARG ++ moveSP ++ restore "THAT" 
                           ++ restore "THIS" ++ restore "ARG" ++ restore "LCL"
                           ++ gotoRet
  where backupLCL = "@LCL\nD=M\n@R13\nM=D\n"
        backupRet = "@5\nA=D-A\nD=M\n@R14\nM=D\n"
        popToARG = pop "M" ++ "@ARG\nA=M\nM=D\n"
        moveSP = "@ARG\nD=M+1\n@SP\nM=D\n"
        restore name = "@R13\nAM=M-1\nD=M\n@" ++ name ++ "\nM=D\n"
        gotoRet = "@R14\nA=M\n0;JMP\n"

parse' op [] n fileName _
  | op `elem` ["add", "sub", "and", "or"] = pop "M" ++ pop ('M':(f op):"D") ++ push "D"
  | op `elem` ["eq", "gt", "lt"] = pop "M" ++ pop "M-D" ++ ifThenElse op n fileName
  | otherwise = pop "M" ++ push (f op:"D")
    where f "add" = '+'; f "sub" = '-'; f "and" = '&'; 
          f "or" = '|'; f "neg" = '-'; f "not" = '!'

parse' "push" ["constant", x] _ _ _ = "@" ++ x ++ "\nD=A\n" ++ push "D"

parse' cmd [seg, x] _ _ _ | seg `elem` ["local", "argument", "this", "that"] =
  case cmd of 
    "push" -> getAddr seg x ++ "A=D\nD=M\n" ++ push "D"
    "pop" -> getAddr seg x ++ "@R13\nM=D\n" ++ pop "M" ++ "@R13\nA=M\nM=D\n"

parse' cmd [seg, x] _ _ _ | seg `elem` ["pointer", "temp"] = 
  case cmd of 
    "push" -> getAddr' seg x ++ "D=M\n" ++ push "D"
    "pop" -> pop "M" ++ getAddr' seg x ++ "M=D\n"

parse' cmd ["static", x] _ filename _ =
  case cmd of
    "push" -> getAddr'' x filename ++ "D=M\n" ++ push "D"
    "pop" -> pop "M" ++ getAddr'' x filename ++ "M=D\n"

parse' "label" [x] _ _ fName = "(" ++ fName ++ "$" ++ x ++ ")\n"

parse' "goto" [x] _ _ fName = "@" ++ fName ++ "$" ++ x ++ "\n0;JMP\n" 

parse' "if-goto" [x] _ _ fName = pop "M" ++ "@" ++ fName ++ "$" ++ x ++ "\nD;JNE\n"

parse' "function" [f, n] _ _ _ = "(" ++ f ++ ")\n" ++ (mconcat $ replicate (read n) $ push "0")

parse' "call" [f, m] n _ _ = pushRet ++ save "LCL" ++ save "ARG" ++ save "THIS" 
                             ++ save "THAT" ++ setLCL ++ setARG ++ gotoF ++ placeRet
  where pushRet = "@RET" ++ show n ++ "\nD=A\n" ++ push "D"
        save x = "@" ++ x ++ "\nD=M\n" ++ push "D"
        setLCL = "@SP\nD=M\n@LCL\nM=D\n"
        setARG = "@5\nD=D-A\n@" ++ m ++ "\nD=D-A\n@ARG\nM=D\n"
        placeRet = "(RET" ++ show n ++ ")\n"
        gotoF = "@" ++ f ++ "\n0;JMP\n" 
--To do the return-addr: generate label RET582 if say n==582, place the label after the goto f jump

getAddr seg x = "@" ++ seg2Lab seg ++ "\nD=M\n@" ++ x ++ "\nD=A+D\n"

getAddr' seg x = "@" ++ show (read x + (if seg == "pointer" then 3 else 5)) ++ "\n"

getAddr'' x filename = "@" ++ filename ++ "." ++ x ++ "\n"

seg2Lab seg = case seg of
  "local" -> "LCL"
  "argument" -> "ARG"
  "this" -> "THIS" 
  "that" -> "THAT"

push :: [Char] -> [Char]
push xs = "@SP\nA=M\nM=" ++ xs ++ "\n@SP\nM=M+1\n"

-- should change @label to @IFlabel
ifThenElse :: [Char] -> Int -> [Char] -> [Char]
ifThenElse cond n fileName = "@IF" ++ label ++ "\nD;J" ++ cond' ++ "\n" ++ push "0"
                             ++ "@ENDIF" ++ label ++ "\n0;JMP\n(IF" ++ label ++ ")\n" 
                             ++ push "-1" ++ "(ENDIF" ++ label ++ ")\n"
  where label = fileName ++ show n
        cond' = toUpper <$> cond

pop :: [Char] -> [Char]
pop xs = "@SP\nAM=M-1\nD=" ++ xs ++ "\n"

parseline :: [[Char]] -> Int -> [Char] -> [Char] -> [Char] -> [Char]
parseline [] _ acc _ _ = acc
parseline (line:lines) n acc filename funName = parseline lines (n + 1) (acc ++ parse' cmd target n filename funName') filename funName'
  where cmd:target = words line
        funName' = if cmd == "function" then head target else funName

stripJunk :: [Char] -> [[Char]]
stripJunk = filter (not . isEmptyLine) . fmap (head . splitOn "//") . lines . replCrWithNl

isEmptyLine :: [Char] -> Bool
isEmptyLine = null . filter (not . flip elem " \t") 

parseCode :: [Char] -> [Char] -> [Char]
parseCode xs filename = (parseline (stripJunk xs) 0 "" filename "")

parseCodes :: [[Char]] -> [[Char]] -> [Char]
parseCodes codes filenames = preamble ++ (mconcat $ zipWith parseCode codes filenames) ++ epilogue

replCrWithNl :: [Char] -> [Char]
replCrWithNl = fmap cr2nl 
  where cr2nl '\r' = '\n'
        cr2nl c = c

lastSplit c xs = (take (prefix - 1) xs, drop prefix xs)
  where prefix = length xs - (fromJust . elemIndex c . reverse) xs


-- assuming the input is a dir with a trailing '/'
main = do
  dir <- head <$> getArgs
  filesWODir <- filter isVMfile <$> listDirectory dir 
  let vmFiles = (dir++) <$> filesWODir
  let ofPath = dir ++ (snd $ lastSplit '/' $ init dir) ++ ".asm"
  let filenames = removeExt <$> filesWODir
  codes <- sequence $ readFile <$> vmFiles
  writeFile ofPath $ parseCodes codes filenames
    where isVMfile xs = drop (length xs - 3) xs == ".vm"
          removeExt xs = take (length xs - 3) xs
