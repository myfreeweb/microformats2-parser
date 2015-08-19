{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax, CPP #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec
-- import           TestCommon
-- import           Data.Microformats2.Parser
#if __GLASGOW_HASKELL__ < 709
-- import           Control.Applicative
#endif

spec ∷ Spec
spec = do
  describe "parseMf2" $ do
    it "works" pending
    -- let parseMf2' = parseMf2 Unsafe . documentRoot . parseLBS
