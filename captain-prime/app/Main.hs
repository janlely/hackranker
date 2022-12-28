{-# LANGUAGE MultiWayIf #-}
module Main where

import Data.Vector.Unboxed.Mutable ( replicate, read, write )
import Control.Monad (when, forM_, replicateM_)
import Prelude hiding (read, replicate)

main :: IO ()
main = do
    v <- replicate 1000001 True
    write v 0 False
    write v 1 False
    forM_ [2..1000000] $ \i -> do
        read v i >>= (`when` do
            forM_ [i*2, i*3..1000000] $ \j -> do
                write v j False)
    let isLeft :: Int -> Int -> IO Bool
        isLeft 0 _ = return True
        isLeft i e
          | i == 0 = return True
          | i `div` e < 1 = return False
          | otherwise = do
              b <- read v i
              if b
              then isLeft (i `mod` e) (e `div` 10)
              else return False
        isRight :: Int -> IO Bool
        isRight 0 = return True
        isRight i
          | i == 0 = return True
          | i `mod` 10 == 0 = return False
          | otherwise = do
              b <- read v i
              if b
              then isRight (i `div` 10)
              else return False
        findHigh :: Int -> Int -> Int
        findHigh i e = if i > 10
                         then findHigh (i `div` 10) (e * 10)
                         else e
        classify :: Int -> IO String
        classify i = do
            l <- isLeft i (findHigh i 1)
            r <- isRight i
            if | l && r -> return "CENTRAL"
               | l -> return "LEFT"
               | r -> return "RIGHT"
               | otherwise -> return "DEAD"
    t <- readLn
    replicateM_ t $ do
        n <- readLn
        classify n >>= putStrLn
