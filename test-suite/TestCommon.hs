{-# LANGUAGE UnicodeSyntax #-}

module TestCommon (
  xml
, documentRoot
, parseLBS
) where

import           Text.RawString.QQ
import           Language.Haskell.TH.Quote
import           Text.XML.Lens (documentRoot)
import           Text.HTML.DOM

xml ∷ QuasiQuoter
xml = r -- vim
