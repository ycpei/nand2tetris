import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)

parseLine :: Map [Char] [Char] -> [Char] -> [Char]
parseLine table (x:xs)
  | x == '@' && (head xs `elem` ['0'..'9']) = int2Bin16 $ read xs
  | x == '@' = table Map.! xs
  | '=' `elem` xs = let [dests, alu] = splitOn "=" (x:xs) in "111" ++ (parseALU alu) ++ (parseDests dests) ++ "000"
  | ';'

parseALU xs = case xs of
  "0" -> "0101010"
  "1" -> "0111111"
  "-1" -> "0111010"
  "D" -> "0001100"
  "A" -> "0110000"
  "!D" -> "0001101"
  "!A" -> "0110001"
  "-D" -> "0001111"
  "-A" -> "0110011"
  "D+1" -> "0011111"
  "A+1" -> "0110111"
  "D-1" -> "0001110"
  "A-1" -> "0110010"
  "D+A" -> "0000010"
  "A+D" -> "0000010"
  "D-A" -> "0010011"
  "A-D" -> "0000111"
  "D&A" -> "0000000"
  "A&D" -> "0000000"
  "D|A" -> "0010101"
  "A|D" -> "0010101"
  "M" -> "1110000"
  "!M" -> "1110001"
  "-M" -> "1110011"
  "M+1" -> "1110111"
  "M-1" -> "1110010"
  "D+M" -> "1000010"
  "M+D" -> "1000010"
  "D-M" -> "1010011"
  "M-D" -> "1000111"
  "D&M" -> "1000000"
  "M&D" -> "1000000"
  "D|M" -> "1010101"
  "M|D" -> "1010101"


parseDests xs = (f 'A'):(f 'D'):(f 'M'):[]
  where f x = if x `elem` xs then '1' else '0'

int2Bin x = showIntAtBase 2 intToDigit x ""

int2Bin16 :: Int -> [Char]
int2Bin16 x = let xs = int2Bin x in replicate (16 - length xs) '0' ++ xs

initTable :: Map [Char] [Char]
initTable = Map.fromList $ (\(x, y) -> (x, int2Bin16 y)) <$> ("SP", 0):("LCL", 1):("ARG", 2):("THIS", 3):("THAT", 4):("SCREEN", 16384):("KBD", 24576):rs
  where rs = zipWith (,) ((\x -> 'R':(show x)) <$> [0..15]) [0..15]

stripJunk :: [Char] -> [[Char]]
stripJunk xs = filter (not . null) $ (filter (not . (flip elem " \t")) . head . (splitOn "//")) <$> splitOn "\n" xs

addSyms :: [[Char]] -> Int -> Int -> Map [Char] [Char] -> Map [Char] [Char]
addSyms [] _ _ table = table
addSyms ((hd:tl):rest) addr vaddr table
  | hd == '(' = addSyms rest addr vaddr (Map.insert (init tl) (int2Bin16 addr) table)
  | hd == '@' && tl `Map.notMember` table = addSyms rest (addr + 1) (vaddr + 1) (Map.insert tl (int2Bin16 vaddr) table)
  | otherwise = addSyms rest (addr + 1) vaddr table

--addSyms (stripJunk input) 0 16 initTable
