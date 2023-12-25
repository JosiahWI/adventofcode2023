module Main ( main ) where

import Data.Maybe (catMaybes)
import Josiah.AOC.Parser
import Text.Trifecta
import System.Environment (getArgs)

newtype Ray = Ray (Vector, Vector)
                deriving Show

main :: IO ()
main = do
  filename <- head <$> getArgs
  result <- solve filename
  print result

solve :: FilePath -> IO Int
solve filename = do
  content <- readFile filename
  case parseString vectorPairLines mempty content of
    Failure msg -> fail $ show msg
    Success hail -> return $ countIntersections (map toRay hail)

countIntersections :: [Ray] -> Int
countIntersections []     = 0
countIntersections (v:vs) = these + those
  where these = length
              $ filter inBounds
              $ catMaybes
              $ map (futureIntersection v) vs
        those = countIntersections vs

toRay :: (Vector, Vector) -> Ray
toRay = Ray

inBounds :: (Double, Double) -> Bool
inBounds (x, y) =  (x >= min') && (x <= max')
                && (y >= min') && (y <= max')
                where min' = 200000000000000.0
                      max' = 400000000000000.0

futureIntersection :: Ray -> Ray -> Maybe (Double, Double)
futureIntersection r1 r2 = 
  intersection r1 r2 >>= checkBoth r1 r2
  where checkBoth :: Ray -> Ray -> (Double, Double) -> Maybe (Double, Double)
        checkBoth r1 r2 p
          | check r1 p && check r2 p = Just p
          | otherwise                = Nothing
        check :: Ray -> (Double, Double) -> Bool
        check r@(Ray (Vector x y _, Vector dx dy _)) (x', y')
          | dx > 0 && x' < x = False
          | dx < 0 && x' > x = False
          | dy > 0 && y' < y = False
          | dy < 0 && y' > y = False
          | otherwise        = True

intersection :: Ray -> Ray -> Maybe (Double, Double)
intersection r1@(Ray (Vector x1 y1 _, Vector dx1 dy1 _))
             r2@(Ray (Vector x2 y2 _, Vector dx2 dy2 _))
  -- We might have to watch out for floating point error.
  | m1 /= m2  = let x = (b2 - b1) / (m1 - m2)
                in Just (x, m1 * x + b1)
  | otherwise = Nothing
  where m1 = dy1 / dx1
        m2 = dy2 / dx2
        b1 = y1 - m1 * x1
        b2 = y2 - m2 * x2
        tX = (x1 - x2) / (dx2 - dx1)
        tY = (y1 - y2) / (dy2 - dy1)
