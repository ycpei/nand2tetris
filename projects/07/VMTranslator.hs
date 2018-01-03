import Data.Char (toUpper)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

preamble = "@256\nD=A\n@SP\nM=D\n"

epilogue = "(END)\n@END\n0;JMP"

parse' :: [Char] -> [[Char]] -> Int -> [Char] -> [Char]
parse' op [] n _
  | op `elem` ["add", "sub", "and", "or"] = pop "M" ++ pop ('M':(f op):"D") ++ push "D"
  | op `elem` ["eq", "gt", "lt"] = pop "M" ++ pop "M-D" ++ ifThenElse op n
  | otherwise = pop "M" ++ push (f op:"D")
    where f "add" = '+'; f "sub" = '-'; f "and" = '&'; 
          f "or" = '|'; f "neg" = '-'; f "not" = '!'

parse' "push" ["constant", x] _ _ = "@" ++ x ++ "\nD=A\n" ++ push "D"

parse' cmd [seg, x] _ _ | seg `elem` ["local", "argument", "this", "that"] =
  case cmd of 
    "push" -> getAddr seg x ++ "A=D\nD=M\n" ++ push "D"
    "pop" -> getAddr seg x ++ "@R13\nM=D\n" ++ pop "M" ++ "@R13\nA=M\nM=D\n"

parse' cmd [seg, x] _ _ | seg `elem` ["pointer", "temp"] = 
  case cmd of 
    "push" -> getAddr' seg x ++ "D=M\n" ++ push "D"
    "pop" -> pop "M" ++ getAddr' seg x ++ "M=D\n"

parse' cmd ["static", x] _ filename =
  case cmd of
    "push" -> getAddr'' x filename ++ "D=M\n" ++ push "D"
    "pop" -> pop "M" ++ getAddr'' x filename ++ "M=D\n"

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

ifThenElse :: [Char] -> Int -> [Char]
ifThenElse cond n = "@" ++ cond' ++ show n ++ "\nD;J" ++ cond' ++ "\n" ++ push "0"
                  ++ "@ENDIF" ++ cond' ++ show n ++ "\n0;JMP\n(" ++ cond' ++ show n ++ ")\n" 
                  ++ push "-1" ++ "(ENDIF" ++ cond' ++ show n ++ ")\n"
  where cond' = toUpper <$> cond

pop :: [Char] -> [Char]
pop xs = "@SP\nAM=M-1\nD=" ++ xs ++ "\n"

parseline :: [[Char]] -> Int -> [Char] -> [Char] -> [Char]
parseline [] _ acc _ = acc
parseline (line:lines) n acc filename = parseline lines (n + 1) (acc ++ parse' cmd target n filename) filename
  where cmd:target = words line

stripJunk :: [Char] -> [[Char]]
stripJunk = filter (not . isEmptyLine) . fmap (head . splitOn "//") . lines . replCrWithNl

isEmptyLine :: [Char] -> Bool
isEmptyLine = null . filter (not . flip elem " \t") 

parseCode :: [Char] -> [Char] -> [Char]
parseCode xs filename = preamble ++ (parseline (stripJunk xs) 0 "" filename) ++ epilogue

replCrWithNl :: [Char] -> [Char]
replCrWithNl = fmap cr2nl 
  where cr2nl '\r' = '\n'
        cr2nl c = c

lastSplit c xs = (take (prefix - 1) xs, drop prefix xs)
  where prefix = length xs - (fromJust . elemIndex c . reverse) xs

main = do
  args <- getArgs
  let path = head args
  let pathWithoutExt = fst $ lastSplit '.' path
  let filename = snd $ lastSplit '/' pathWithoutExt
  code <- readFile path
  writeFile (pathWithoutExt ++ ".asm") (parseCode code filename)
