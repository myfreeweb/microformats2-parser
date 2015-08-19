{-# LANGUAGE UnicodeSyntax #-}

module TestCommon (
  xml
, json
, def
, documentRoot
, parseLBS
) where

import           Language.Haskell.TH.Quote
import           Text.RawString.QQ
import           Text.XML.Lens (documentRoot)
import           Text.HTML.DOM
import           Data.Aeson.QQ
import           Data.Default

-- renames for vim
xml, json ∷ QuasiQuoter
xml = r
json = aesonQQ
