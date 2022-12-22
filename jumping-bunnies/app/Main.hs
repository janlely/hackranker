module Main where

main :: IO ()
main = do
    _ <- getLine
    js <- fmap read . words <$> getLine
    print $ foldl1 lcm js

