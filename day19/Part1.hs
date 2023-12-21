module Main
       ( main )
       where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Trifecta
import System.Environment (getArgs)

type Comparison = (Integer -> Integer -> Bool)
type Label = String
type Part  =
  (Integer, Integer, Integer, Integer)
type Workstation = (Label, [Rule])

data Category = X | M | A | S
  deriving Show

data Rule =
    Simple Target
  | Complex Category Comparison Integer Target
    deriving Show

data Target = Accept | Reject | Goto Label
  deriving (Eq, Show)

instance Show (a -> b) where
  show _ = "<()>"

main :: IO ()
main = do
  filename <- head <$> getArgs
  solve filename >>= print

solve :: FilePath -> IO Integer
solve filename = do
  content <- readFile filename
  let (workstations, parts) =
        case parseString parseInput mempty content of
          Success s -> s
          Failure f  -> error (show f)
      start          = "in"
      workstationMap = Map.fromList workstations
      solver         = solvePart workstationMap start
  return $ sum $ map rate $ filter (\part -> (solver part) == Accept) parts

solvePart :: Map Label [Rule] -> Label -> Part -> Target
solvePart workstationMap start part
  = head 
    $ dropWhile (\x -> (x /= Accept && x /= Reject))
    $ iterate (nextTarget workstationMap part) (Goto start)

nextTarget :: Map Label [Rule] -> Part -> Target -> Target
nextTarget workstationMap part target
  = part `matchRule` rules
    where rules = workstationMap ! (labelOf target)

rate :: Part -> Integer
rate (x, m, a, s) = x + m + a + s

parseInput :: Parser ([Workstation], [Part])
parseInput = do
  workstations <- some workstation
  char '\n'
  parts <- some part
  return (workstations, parts)

workstation :: Parser Workstation
workstation = do
  label <- some letter
  char '{'
  rules <- sepBy rule (char ',')
  string "}\n"
  return (label, rules)

rule :: Parser Rule
rule = do
      try parseComplexRule
  <|> parseSimpleRule

parseCategory :: Parser Category
parseCategory = do
      (const X <$> char 'x')
  <|> (const M <$> char 'm')
  <|> (const A <$> char 'a')
  <|> (const S <$> char 's')

parseComparison :: Parser Comparison
parseComparison = do
      (const (>)  <$> char '>')
  <|> (const (<)  <$> char '<')
  <|> (const (==) <$> char '=')

parseSimpleRule :: Parser Rule
parseSimpleRule = do
  Simple <$> parseTarget

parseComplexRule :: Parser Rule
parseComplexRule = do
  category <- parseCategory
  ordering <- parseComparison
  measure  <- integer
  char ':'
  target <- parseTarget
  return $ Complex category ordering measure target

parseTarget :: Parser Target
parseTarget = do
      (const Accept <$> char 'A')
  <|> (const Reject <$> char 'R')
  <|> (Goto <$> some letter)

part :: Parser Part
part = do
  char '{'
  (x:m:a:s:_) <- sepBy parseQty (char ',')
  string "}\n"
  return (x, m, a, s)
  where parseQty :: Parser Integer
        parseQty = do
          parseCategory
          char '='
          integer

matchRule :: Part -> [Rule] -> Target
matchRule part rules =
  fromJust $ firstJust $ map (match part) rules
  where match :: Part -> Rule -> Maybe Target
        match part (Simple target) = Just target
        match (x, m, a, s)
                (Complex category comp n target)
                  = let check y = if y `comp` n then Just target else Nothing
                    in case category of
                      X -> check x
                      M -> check m
                      A -> check a
                      S -> check s

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (j@(Just _):_) = j
firstJust (x:xs)         = firstJust xs

labelOf :: Target -> Label
labelOf (Goto l) = l
labelOf (Accept) = "A"
labelOf (Reject) = "R"
