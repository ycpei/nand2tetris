import Data.Char (toUpper)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

preamble = "@256\nD=A\n@SP\nM=D\n"

epilogue = "(END)\n@END\n0;JMP"

parse' :: [Char] -> [[Char]] -> Int -> [Char]
parse' op [] n
  | op `elem` ["add", "sub", "and", "or"] = pop "M" ++ pop ('M':(f op):"D") ++ push "D"
  | op `elem` ["eq", "gt", "lt"] = pop "M" ++ pop "M-D" ++ ifThenElse op n
  | otherwise = pop "M" ++ push (f op:"D")
    where f "add" = '+'; f "sub" = '-'; f "and" = '&'; 
          f "or" = '|'; f "neg" = '-'; f "not" = '!'

parse' "push" ["constant", x] _ = "@" ++ x ++ "\nD=A\n" ++ push "D"
parse' cmd [seg, x] _ | seg `elem` ["local", "argument", "this", "that"] =
  case cmd of 
    | "push" -> getAddr seg x ++ push "D"
    | "pop" -> getAddr seg x ++ "@R13\nM=D\n" ++ pop "M" ++ "@R13\nA=M\nM=D\n"

parse' cmd [seg, x] _ | seg `elem` ["pointer", "temp"] = 
  case cmd of 
    | "push" -> getAddr' seg x ++ "D=M\n" ++ push "D"
    | "pop" -> pop "M" ++ getAddr' seg x ++ "M=D\n"

parse' cmd ["static", x] _ =
  case cmd of
    | "push" -> getAddr'' x ++ "D=M\n" ++ push "D"
    | "pop" -> pop "M" ++ getAddr'' x ++ "M=D\n"

getAddr seg x = "@" ++ seg2Lab seg ++ "\nD=M\n@" ++ x ++ "\nA=D+M\nD=M\n"

getAddr' seg x = "@" ++ show (read x + (if seg == "pointer" then 3 else 5)) ++ "\n"

getAddr'' x = "@" ++ filename ++ "." ++ x ++ "\n"

seg2Lab seg = case seg of
  | "local" -> "LCL"
  | "argument" -> "ARG"
  | "this" -> "THIS" 
  | "that" -> "THAT"

push :: [Char] -> [Char]
push xs = "@SP\nA=M\nM=" ++ xs ++ "\n@SP\nM=M+1\n"

ifThenElse :: [Char] -> Int -> [Char]
ifThenElse cond n = "@" ++ cond' ++ show n ++ "\nD;J" ++ cond' ++ "\n" ++ push "0"
                  ++ "@ENDIF" ++ cond' ++ show n ++ "\n0;JMP\n(" ++ cond' ++ show n ++ ")\n" 
                  ++ push "-1" ++ "(ENDIF" ++ cond' ++ show n ++ ")\n"
  where cond' = toUpper <$> cond

pop :: [Char] -> [Char]
pop xs = "@SP\nAM=M-1\nD=" ++ xs ++ "\n"

parseline :: [[Char]] -> Int -> [Char] -> [Char]
parseline [] _ acc = acc
parseline (line:lines) n acc = parseline lines (n + 1) (acc ++ parse' cmd target n)
  where cmd:target = words line

stripJunk :: [Char] -> [[Char]]
stripJunk = filter (not . isEmptyLine) . fmap (head . splitOn "//") . lines . replCrWithNl

isEmptyLine :: [Char] -> Bool
isEmptyLine = null . filter (not . flip elem " \t") 

parseCode :: [Char] -> [Char]
parseCode xs = preamble ++ (parseline (stripJunk xs) 0 "") ++ epilogue

replCrWithNl :: [Char] -> [Char]
replCrWithNl = fmap cr2nl 
  where cr2nl '\r' = '\n'
        cr2nl c = c

--input = "push constant 17\npush constant 17\neq"

main = do
  args <- getArgs
  let filename = head args
  code <- readFile $ filename
  writeFile (head (splitOn "." filename) ++ ".asm") (parseCode code)
  --}