module Main (main) where

data TestCase = TestCase
  { 
    markdown :: String,
    html :: String,
    example :: Int,
    section :: String
  }

main :: IO ()
main = putStrLn "Test suite not yet implemented."
