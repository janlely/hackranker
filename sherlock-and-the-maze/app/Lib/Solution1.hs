{-# LANGUAGE BangPatterns #-}
module Lib.Solution1 (solution) where 

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Bifunctor (first)
import Control.Monad (replicateM_)
import Data.Ix (range)
import Data.List (foldl')

solution :: IO ()
solution = do
    t <- readLn
    let !answers = solve 100
    replicateM_ t $ do
        [n, m, k] <- map read . words <$> getLine
        let Answer fu fl =  answers V.! index 100 (n-1,m-1)
        print $ VU.foldl1 add (VU.take (k+1) fu) `add` VU.foldl1 add (VU.take (k+1) fl)
        

data Answer = Answer
  { _fromUp :: VU.Vector Int
  , _fromLeft :: VU.Vector Int
  } deriving (Show)

start = Answer (VU.singleton 1) (VU.singleton 1)

empty = Answer VU.empty VU.empty

index :: Int -> (Int, Int) -> Int
index n (i,j) = i * n + j

add :: Int -> Int -> Int
add a b = (a + b) `mod` 1000000007


solve :: Int -> V.Vector Answer
solve n = foldl' f vi $ tail $ range ((0,0), (n-1,n-1)) 
  where vi = V.fromList $ start : replicate (n*n - 1) empty
        f v (i,j)
          | i == 0 = v V.// [(index n (i,j), Answer VU.empty (VU.singleton 1))]
          | j == 0 = v V.// [(index n (i,j), Answer (VU.singleton 1) VU.empty)]
          | otherwise = v V.// [(index n (i,j), newAnswer)]
          where newAnswer = let Answer fu fl = v V.! index n (i-1,j)
                                Answer fu' fl' = v V.! index n (i,j-1)
                                fuRes = VU.replicate (max (VU.length fu) (VU.length fl + 1)) 0
                                flRes = VU.replicate (max (VU.length fl') (VU.length fu' + 1)) 0
                             in Answer
                                  (let fu1 = VU.accumulate add fuRes (VU.indexed fu)
                                    in VU.accumulate add fu1 (VU.map (first (+1)) (VU.indexed fl)))
                                  (let fl1 = VU.accumulate (+) flRes (VU.indexed fl')
                                    in VU.accumulate add fl1 (VU.map (first (+1)) (VU.indexed fu')))