{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}

module Data.Microformats2.Parser.HtmlUtilSpec (spec) where

import           Prelude.Compat
import           Test.Hspec
import           TestCommon
import           Data.Microformats2.Parser.HtmlUtil

spec ∷ Spec
spec = do
  describe "getInnerTextRaw" $ do
    it "returns textContent without handling imgs" $ do
      let txtraw = getInnerTextRaw . documentRoot . parseLBS
      txtraw [xml|<div>This is <a href="">text content</a> <img src="/yo" alt="NOPE"> without any stuff.
  	</div>|] `shouldBe` Just "This is text content  without any stuff."

  describe "getInnerTextWithImgs" $ do
    it "returns textContent with handling imgs" $ do
      let txtraw = getInnerTextWithImgs . documentRoot . parseLBS
      txtraw [xml|<div>This is <a href="">text content</a> <img src="/yo" alt="with an alt"> <img src="and-src">.
    </div>|] `shouldBe` Just "This is text content with an alt and-src."

  describe "getInnerHtmlSanitized" $ do
    it "doesn't eat custom properties but eats scripts" $ do
      let geth = getInnerHtmlSanitized Nothing . documentRoot . parseLBS
      geth [xml|<html><custom-element onload="alert(2)">memes<script>alert(1)</script></custom-element>|]
        `shouldBe` Just "<custom-element>memes</custom-element>"
