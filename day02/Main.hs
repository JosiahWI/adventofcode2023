module Main
       ( main )
       where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO

data CubeSet =
  CubeSet { red   :: Int
          , green :: Int
          , blue  :: Int
          } deriving Show

type Game   = [CubeSet]
type GameID = Int

instance Semigroup CubeSet where
  (CubeSet r g b) <> (CubeSet r' g' b') = CubeSet (max r r') (max g g') (max b b')

instance Monoid CubeSet where
  mempty = CubeSet 0 0 0

main :: IO ()
main = do
  args <- getArgs
  let filepath = args !! 0
  games <- (map parseGame . lines) <$> readFile filepath

  hPutStrLn stderr $ show (length games) ++ " games loaded"

  print $ sum $ map (power . smallestPossibleSet) games

keepKeyBy :: (b -> Bool) -> (a, b) -> Maybe a
keepKeyBy f (a, b) = if f b then Just a else Nothing

power :: CubeSet -> Int
power (CubeSet r g b) = r * g * b

smallestPossibleSet :: Game -> CubeSet
smallestPossibleSet = mconcat

isPossibleGame :: Game -> Bool
isPossibleGame = all isPossibleSet
                   where isPossibleSet :: CubeSet -> Bool
                         isPossibleSet (CubeSet r g b) =
                             r <= 12 && g <=13 && b <= 14

parseGame :: String -> Game
parseGame s = let parts  = splitOn ":" s
                  body   = parts !! 1
                  sets   = map parseSet $ splitOn ";" body
               in sets

parseSet :: String -> CubeSet
parseSet = mconcat . map (fromJust . fromString) . splitOn ","

fromString :: String -> Maybe CubeSet
fromString s = do
  (count, color) <- parseCubeCount s
  case color of
    "red"   -> Just (CubeSet count 0 0)
    "green" -> Just (CubeSet 0 count 0)
    "blue"  -> Just (CubeSet 0 0 count)
    _       -> Nothing

parseTuple :: String -> Maybe (String, String)
parseTuple s = case words s of
                   [a, b] -> Just (a, b)
                   _      -> Nothing

parseCubeCount :: String -> Maybe (Int, String)
parseCubeCount s = do
  (a, b) <- parseTuple s
  if all isDigit a then Just (read a, b) else Nothing
