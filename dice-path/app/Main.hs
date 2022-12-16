module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"


data Dice = Dice
  { _top :: Int,
  , _bottom :: Int
  , _front :: Int,
  , _back :: Int
  , _left :: Int
  , _right :: Int
  } deriving (Show)

