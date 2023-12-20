module Main
       ( main )
       where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

digitNames :: [String]
digitNames =
  [ "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

digitNumerals :: [String]
digitNumerals = map pure ['1'..'9']

getDigits :: String -> String
getDigits = filter isDigit

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (not . null) . lines

solveLine :: String -> Int
solveLine s =
  let nsHead = (getDigits . fixFirst) s
      nsTail = (getDigits . fixLast) s
  in read ([head nsHead, last nsTail])

findReplacement :: String -> [(String, String)]-> Maybe (String, String)
findReplacement haystack pairs  =
  case filter ((`isPrefixOf` haystack) . fst) pairs of
    [pair] -> Just pair
    _        -> Nothing

replaceAtHead :: String -> (String, String) -> String
replaceAtHead haystack (from, to) = to ++ (drop (length from) haystack) 

fixFirst :: String -> String
fixFirst "" = ""
fixFirst s =
  case findReplacement s (zip digitNames digitNumerals) of
    Just needle -> replaceAtHead s needle
    Nothing     -> head s : fixFirst (tail s)

fixLast :: String -> String
fixLast s = reverse $ go (reverse s)
  where go :: String -> String
        go "" = ""
        go s  =
          case findReplacement s (zip (map reverse digitNames) digitNumerals) of
            Just needle -> replaceAtHead s needle
            Nothing     -> head s : go (tail s)
       
main :: IO ()
main = do
  content <- readFile "input.txt"
  let result = sum $ map solveLine $ nonEmptyLines content
  print $ result
