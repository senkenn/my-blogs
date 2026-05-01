module MdCore (markdownToHtml) where

import Data.Text (Text)

markdownToHtml :: Text -> Text
markdownToHtml = id
