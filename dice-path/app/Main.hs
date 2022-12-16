module Main where

import qualified Data.Vector as V
import Control.Monad (replicateM_, forM_)
import Data.Maybe (fromJust)

main :: IO ()
main = do
--   t <- readLn
  let v = solve 60 
  forM_ [0..2] $ \i -> do
    forM_ [0..2] $ \j -> do
        putStr (show i ++ "," ++ show j ++ ": ")
        print $ v V.! index 60 (i,j)
--   replicateM_ t $ do
--     [i,j] <- fmap read . words <$> getLine
--     print $ v V.! index 60 (i-1,j-1)

data Dice = Dice
  { top :: Int
  , bottom :: Int
  , left :: Int
  , right :: Int
  , front :: Int
  , back :: Int
  } deriving (Show)

index :: Int -> (Int, Int) -> Int
index n (i,j) = i * n + j

rotateRight :: Dice -> Dice
rotateRight (Dice t b l r f b') = Dice l r b t f b'

rotateDown :: Dice -> Dice
rotateDown (Dice t b l r f b') = Dice b' f l r t b


solve :: Int -> V.Vector (Int, Maybe Dice)
solve n = foldl f vi $ tail [(i,j)| i<- [0..n-1], j <- [0..n-1]]
  where vi = V.generate (n*n) (\k -> if k == 0 then (1, Just (Dice 1 6 3 4 2 5)) else (0, Nothing))
        f v (i,j)
          | i == 0    = v V.// [(index n (i,j), (lv, ld))] 
          | j == 0    = v V.// [(index n (i,j), (uv, ud))] 
          | otherwise = v V.// [(index n (i,j), if lv > uv then (lv, ld) else (uv, ud))] 
          where (lv, ld) = let (m, t) = v V.! index n (i,j-1)
                               dice = rotateRight . fromJust $ t
                            in (m + top dice, Just dice)
                (uv, ud) = let (m, t) = v V.! index n (i-1, j)
                               dice = rotateDown . fromJust $ t
                            in (m + top dice, Just dice)
        -- diceStates = V.fromList
        --   [ Dice 0 0 0 0 0 0 -- unused
        --   , Dice 1 6 3 4 2 5 -- top == 1
        --   , Dice 2 5 3 4 6 1 -- top == 2
        --   , Dice 3 4 6 1 2 5 -- top == 3
        --   , Dice 4 3 1 6 2 5 -- top == 4
        --   , Dice 5 2 3 4 1 6 -- top == 5
        --   , Dice 6 1 4 3 5 2 -- top == 6
        --   ]
    