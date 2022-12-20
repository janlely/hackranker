module Main where

import qualified Data.Vector as V
import Control.Monad (replicateM_)
import qualified Data.List as L

main :: IO ()
main = do
  t <- readLn
  let v = solve 60 
  replicateM_ t $ do
    [i,j] <- fmap read . words <$> getLine
    print $ (L.maximum . fmap fst . (v V.!)) $ index 60 (i-1,j-1)

data Dice = Dice
  { top :: Int
  , bottom :: Int
  , left :: Int
  , right :: Int
  , front :: Int
  , back :: Int
  } deriving (Show, Eq)

index :: Int -> (Int, Int) -> Int
index n (i,j) = i * n + j

rotateRight :: Dice -> Dice
rotateRight (Dice t b l r f b') = Dice l r b t f b'

rotateDown :: Dice -> Dice
rotateDown (Dice t b l r f b') = Dice b' f l r t b


solve :: Int -> V.Vector [(Int, Dice)]
solve n = foldl f vi $ tail [(i,j)| i<- [0..n-1], j <- [0..n-1]]
  where vi = V.generate (n*n) (\k -> if k == 0 then [(1, Dice 1 6 3 4 2 5)] else [])
        f v (i,j)
          | i == 0 = v V.// [(index n (i,j), take6 lv)]
          | j == 0 = v V.// [(index n (i,j), take6 uv)]
          | otherwise = v V.// [(index n (i,j), (L.nub . take6) (lv ++ uv))]
          where lv = let s = v V.! index n (i,j-1)
                      in (take6 . L.map rotateR) s
                uv = let s = v V.! index n (i-1,j)
                      in (take6 . L.map rotateD) s
                rotateR (score, dice) = let dice' = rotateRight dice
                                         in (score + top dice', dice')
                rotateD (score, dice) = let dice' = rotateDown dice
                                         in (score + top dice', dice')
                take6 [] = []
                take6 s = let ms = (L.maximum . fmap fst) s
                           in L.filter ((>= ms - 5) . fst) s
