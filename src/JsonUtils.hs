module JsonUtils
  ( encodingOptions
  ) where

import qualified Data.Char as C
import Data.Aeson (Options(fieldLabelModifier, constructorTagModifier), defaultOptions)

encodeField :: [Char] -> [Char]
encodeField field =
  let
    properField = dropWhile C.isLower field
    (l, tail) = splitAt 1 properField
  in
    fmap C.toLower l ++ tail

encodeTag :: [Char] -> [Char]
encodeTag = fmap C.toLower

encodingOptions :: Options
encodingOptions =
  defaultOptions
    { fieldLabelModifier = encodeField
    , constructorTagModifier = encodeTag
    }