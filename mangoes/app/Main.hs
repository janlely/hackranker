{-# LANGUAGE MultiWayIf #-}
module Main where
import Prelude hiding (sum, take, zipWith, head, drop, length)
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed (Vector, fromList, thaw, sum, freeze, zipWith, last)
import Control.Monad (forM_, when)
-- import Data.Vector.Algorithms.Intro (sort)


-- main :: IO ()
-- main = print "hello world"

main :: IO ()
main = do
    [n,m] <- fmap read . words <$> getLine
    as <- fromList . fmap read . words <$> getLine
    hs <- fromList . fmap read . words <$> getLine
    let f i = do
            vs <- thaw $ zipWith (\a b -> a + b * (i-1)) as hs
            topK n i vs
            (fmap ((<= m) . sum) . freeze . M.take i) vs
    binarySearch f 0 n >>= print

binarySearch f l r
  | r <= l = return l
  | otherwise = do
        let mid = (l + r + 1) `div` 2
        f mid >>= \b -> 
          if b
            then binarySearch f mid r
            else binarySearch f l (mid-1) 

checkHeap n k vs = do
    h <- M.read vs 0
    forM_ [1,2..k-1] $ \i -> do
        v <- M.read vs i
        when (h < v) $ error "heap is invalied1"
    forM_ [k,k+1..n-1] $ \i -> do
        v <- M.read vs i
        when (h > v) $ error "heap is invalied2"


topK n k vs = do
    let siftDown parent end = do
            let l = parent * 2 + 1
                r = l + 1
             in if | r < end -> do
                        v1 <- M.read vs l
                        v2 <- M.read vs r
                        v3 <- M.read vs parent
                        if | v3 > v1 && v3 > v2 -> return ()
                           | v2 > v1            -> M.swap vs parent r >> siftDown r end
                           | otherwise          -> M.swap vs parent l >> siftDown l end 
                   | l < end -> do
                        v1 <- M.read vs l
                        v2 <- M.read vs parent
                        when (v2 < v1) $ M.swap vs parent l >> siftDown l end 
                   | otherwise -> return ()
        heapify i = do
            let i' = i `div` 2
             in forM_ [i'-1, i'-2 .. 0] $ \j -> siftDown j k
        go i = do
            when (i < n) $ do
                h <- M.read vs 0
                v <- M.read vs i
                when (h > v) $ do
                    M.swap vs 0 i
                    siftDown 0 k
                go (i+1)
    heapify k
    go k
