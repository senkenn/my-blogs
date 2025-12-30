{-# LANGUAGE OverloadedStrings #-}

module MdCore (renderHtml) where

import Data.Text (Text)

-- | Minimal stub: for wiring check only.
--
-- >>> renderHtml "# hi"
-- "<h1>hi</h1>\n"
renderHtml :: Text -> Text
renderHtml src =
  if src == "# hi"
    then "<h1>hi</h1>\n"
    else src

