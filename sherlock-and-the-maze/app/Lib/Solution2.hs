module Lib.Solution2 (solution) where 

import Control.Monad (replicateM_)
import Data.Array (array, (!))

solution :: IO ()
solution = do
    t <- readLn
    replicateM_ t $ do
        [n, m, k] <- map read . words <$> getLine
        print $ solve (n,m,k)
        

add :: Int -> Int -> Int
add a b = (a + b) `mod` 1000000007

solve :: (Int, Int, Int) -> Int
solve (i,j,k) = sumK $ answers ! (i,j)
  where sumK ([],[]) = 0
        sumK (us,[]) = foldl1 add $ take (k+1) us
        sumK ([],ls) = foldl1 add $ take (k+1) ls
        sumK (us,ls) = let a = foldl1 add $ take (k+1) us
                           b = foldl1 add $ take (k+1) ls
                        in a `add` b
        zipWith2 a [] = a 
        zipWith2 [] b = b
        zipWith2 (a:as) (b:bs) = add a b : zipWith2 as bs
        calc n m
          | n == 1 = ([], [1])
          | m == 1 = ([1], [])
          |otherwise = let (fu, fl)   = answers ! (n-1, m)
                           (fu', fl') = answers ! (n, m-1)
                        in ( zipWith2 fu (0:fl)
                           , zipWith2 (0:fu') fl'
                           )
        answers = array ((1,1), (i,j)) [((i',j'), calc i' j') | i' <- [1..i], j' <- [1..j]]