{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)

import MdCore (renderHtml)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- TIO.readFile filePath
      let port = 3000
      putStrLn $ "http://127.0.0.1:" <> show port <> "/"
      Warp.run port (app content)
    _ -> putStrLn "Usage: md-preview <file.md>"

app :: Text -> Application
app mdContent _req respond = do
  let html = renderHtml mdContent
      body = LBS.fromStrict $ TE.encodeUtf8 $ wrapHtml html
  respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] body

wrapHtml :: Text -> Text
wrapHtml body =
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body>"
    <> body
    <> "</body></html>"
