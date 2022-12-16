module Main where

import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (runST) 


main :: IO ()
main = do
    n <- readLn
    replicateM_ n $ do
        k <- readLn
        print $ solution1 k
        print $ solution2 k
        

solution2 :: Int -> Int
solution2 i = let v = solve 1000
               in v U.! i


solution1 :: Int -> Int
solution1 i = let v = runST $ do
                        v <- UM.replicate 1001 (0::Int)
                        UM.write v 0 1
                        UM.write v 1 1
                        UM.write v 2 2
                        forM_ [3..1000] $ \i -> do
                            forM_ [1..i] $ \j -> do
                                a <- UM.read v (j-1)
                                b <- UM.read v (i-j)
                                UM.modify v (add (mul a b)) i
                        U.freeze v
                in v U.! i

add :: Int -> Int -> Int
add a b = (a + b) `mod` 100000007

mul :: Int -> Int -> Int
mul a b = (a * b) `mod` 100000007

solve :: Int -> U.Vector Int
solve n = foldl f iv [2..n]
    where f v i = U.accum add v [(i, mul (v U.! (i-j)) (v U.! (j-1))) | j <- [1..i]] 
          iv = U.generate (n+1) (\i -> if i > 1 then 0 else 1) 