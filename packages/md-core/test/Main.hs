module Main (main) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text (Text)
import GHC.Generics (Generic)
import MdCore (markdownToHtml)

data SpecExample = SpecExample
  { markdown :: Text,
    html :: Text,
    example :: Int,
    section :: Text
  }
  deriving (Generic)

instance FromJSON SpecExample

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "test-data/spec.json"
  case result of
    Left err -> error err
    Right [] -> error "spec.json is empty"
    Right (s : _) -> do
      let actual = markdownToHtml (markdown s)
      let expected = html s
      if actual == expected
        then putStrLn "PASS"
        else do
          putStrLn "FAIL"
          putStrLn "  expected:"
          print expected
          putStrLn "  actual:"
          print actual
