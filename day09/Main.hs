module Main
       ( main )
       where

import System.Environment (getArgs)

main :: IO ()
main = do
  filename <- head <$> getArgs
  resultOne <- solve1 filename
  print resultOne
  resultTwo <- solve2 filename
  print resultTwo

solve1 :: FilePath -> IO Int
solve1 filepath = do
  ls <- (map toInts . lines) <$> readFile filepath
  return $ sum $ map nextNumber ls

solve2 :: FilePath -> IO Int
solve2 filepath = do
  ls <- (map toInts . lines) <$> readFile filepath
  return $ sum $ map (nextNumber . reverse) ls

nextNumber :: (Eq a, Num a) => [a] -> a
nextNumber = sum . map last . allDiffs

allDiffs :: (Eq a, Num a) => [a] -> [[a]]
allDiffs xs = takeWhile (not . all (==0)) $ iterate differences xs

differences :: Num a => [a] -> [a]
differences = pairwiseWith $ flip (-)

pairwiseWith :: (a -> a -> b) -> [a] -> [b]
pairwiseWith f (x:xs@(y:_)) = f x y : pairwiseWith f xs
pairwiseWith f _            = []

toInts :: String -> [Int]
toInts = map read . words
