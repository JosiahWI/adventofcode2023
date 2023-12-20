module Main
       ( main )
       where

import Control.Applicative
import Text.Trifecta
import System.Environment (getArgs)

type Label = String
type Part  =
  (Integer, Integer, Integer, Integer)
type Workstation = (Label, [Rule])

data Category = X | M | A | S
  deriving Show

data Rule =
    Simple Target
  | Complex Category Ordering Integer Target
    deriving Show

data Target = Accept | Reject | Goto Label
  deriving Show

main :: IO ()
main = do
  filename <- head <$> getArgs
  solve filename >>= print

solve :: FilePath -> IO Int
solve filename = do
  content <- readFile filename
  let t =
        parseString parseInput mempty content
  print t
  return $ length content

parseInput :: Parser ([Workstation], [Part])
parseInput = do
  workstations <- some workstation
  char '\n'
  parts <- some part
  return (workstations, [])

workstation :: Parser Workstation
workstation = do
  label <- some letter
  char '{'
  rules <- sepBy rule (char ',')
  string "}\n"
  return (label, rules)

rule :: Parser Rule
rule = do
      parseComplexRule
  <|> parseSimpleRule

parseCategory :: Parser Category
parseCategory = do
      (const X <$> char 'x')
  <|> (const M <$> char 'm')
  <|> (const A <$> char 'a')
  <|> (const S <$> char 's')

parseOrdering :: Parser Ordering
parseOrdering = do
      (const GT <$> char '>')
  <|> (const LT <$> char '<')
  <|> (const EQ <$> char '=')

parseSimpleRule :: Parser Rule
parseSimpleRule = do
  target <- parseTarget
  return $ Simple target

parseComplexRule :: Parser Rule
parseComplexRule = do
  category <- parseCategory
  ordering <- parseOrdering
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
