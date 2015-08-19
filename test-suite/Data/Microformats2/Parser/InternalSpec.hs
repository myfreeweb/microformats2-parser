{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes, CPP #-}

module Data.Microformats2.Parser.InternalSpec (spec) where

import           Test.Hspec
import           TestCommon
import           Text.HTML.DOM
import           Text.XML.Lens (documentRoot)
import           Data.Microformats2.Parser.Internal
#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

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

  describe "extractProperty" $ do
    it "parses p- properties" $ do
      let nm = extractProperty P "name" . documentRoot . parseLBS
      nm [xml|<div><span class="p-name">Hello Basic</span>|] `shouldBe` pure "Hello Basic"
      nm [xml|<div><p><span class="p-name">Hello Nested</span>|] `shouldBe` pure "Hello Nested"
      nm [xml|<div><abbr class="p-name" title="Hello Abbr">HA</abbr>|] `shouldBe` pure "Hello Abbr"
      nm [xml|<div><abbr class="p-name">HA</abbr>|] `shouldBe` pure "HA"
      nm [xml|<div><data class="p-name" value="Hello Data" />|] `shouldBe` pure "Hello Data"
      nm [xml|<div><input class="p-name" value="Hello Input" />|] `shouldBe` pure "Hello Input"
      nm [xml|<div><img class="p-name" alt="Hello Img" />|] `shouldBe` pure "Hello Img"
      nm [xml|<div><area class="p-name" alt="Hello Area" />|] `shouldBe` pure "Hello Area"
      nm [xml|<div><span class="p-name"> ignore <i class="value">Hello</i>  <img class="value" alt="ValuePattern" src="x.png"> </span>|] `shouldBe` pure "HelloValuePattern"
      nm [xml|<div><span class="p-name"> ignore <em class="value-title" title="Hello">Hi</em>  <span class="value">Value-Title</span></span>|] `shouldBe` pure "HelloValue-Title"
      nm [xml|<div><span class="p-name">  Hello <img alt="Span With Img" src="x.png"> </span>|] `shouldBe` pure "Hello Span With Img"
      nm [xml|<div><span class="p-name">	Hello <img src="span-with.png"> </span>|] `shouldBe` pure "Hello span-with.png"
      nm [xml|<div><span class="p-name"> <span class="value"> 	Hello 	
  <img alt="Span With Img" src="x.png"> </span> 	<em class="value-title" title="&& Value Title">nope</em> </span>|] `shouldBe` pure "Hello&& Value Title"

    it "parses u- properties" $ do
      let ur = extractProperty U "url" . documentRoot . parseLBS
      ur [xml|<div><a class="u-url" href="/yo/a">link</a>|] `shouldBe` pure "/yo/a"
      ur [xml|<div><area class="u-url" href="/yo/area"/>|] `shouldBe` pure "/yo/area"
      ur [xml|<div><img class="u-url" src="/yo/img"/>|] `shouldBe` pure "/yo/img"
      ur [xml|<div><audio class="u-url" src="/yo/audio"/>|] `shouldBe` pure "/yo/audio"
      ur [xml|<div><video class="u-url" src="/yo/video"/>|] `shouldBe` pure "/yo/video"
      ur [xml|<div><source class="u-url" src="/yo/source"/>|] `shouldBe` pure "/yo/source"
      ur [xml|<div><span class="u-url"><b class=value>/yo</b><em class="value">/vcp</span>|] `shouldBe` pure "/yo/vcp"
      ur [xml|<div><abbr class="u-url" title="/yo/abbr"/>|] `shouldBe` pure "/yo/abbr"
      ur [xml|<div><data class="u-url" value="/yo/data"/>|] `shouldBe` pure "/yo/data"
      ur [xml|<div><input class="u-url" value="/yo/input"/>|] `shouldBe` pure "/yo/input"
      ur [xml|<div><span class="u-url">/yo/span</span>|] `shouldBe` pure "/yo/span"

    it "parses dt- properties" $ do
      let dt = extractProperty Dt "updated" . documentRoot . parseLBS
      dt [xml|<div><time class="dt-updated" datetime="ti.me">someday</time>|] `shouldBe` pure "ti.me"
      dt [xml|<div><ins class="dt-updated" datetime="i.ns">someday</ins>|] `shouldBe` pure "i.ns"
      dt [xml|<div><del class="dt-updated" datetime="d.el">someday</del>|] `shouldBe` pure "d.el"
      dt [xml|<div><abbr class="dt-updated" title="ab.br">AB</abbr>|] `shouldBe` pure "ab.br"
      dt [xml|<div><data class="dt-updated" value="da.ta"/>|] `shouldBe` pure "da.ta"
      dt [xml|<div><input class="dt-updated" value="i.np.ut"/>|] `shouldBe` pure "i.np.ut"
      dt [xml|<div><span class="dt-updated">
            <abbr class="value" title="vcp">VCP</abbr>
            <time class="value" datetime="ti">TIME</time>
            <ins class="value" datetime="me">lol</time>
          </span>|] `shouldBe` pure "vcptime"
      dt [xml|<div><span class="dt-updated">date</span>|] `shouldBe` pure "date"

    it "parses e- properties" $ do
      let ct = extractProperty E "content" . documentRoot . parseLBS
      ct [xml|<div><div class="e-content"><em>hello html</em>!</div>|] `shouldBe` pure "<em>hello html</em>!"

  describe "implyProperty" $ do
    it "parses implied p-name" $ do
      let nm = implyProperty P "name" . documentRoot . parseLBS
      nm [xml|<img class="h-blah" alt="Hello Img!">|] `shouldBe` pure "Hello Img!"
      nm [xml|<abbr class="h-blah" title="Hello Abbr!">HA</abbr>|] `shouldBe` pure "Hello Abbr!"
      nm [xml|<p class="h-blah"><img alt="Hello Only Img!"></p>|] `shouldBe` pure "Hello Only Img!"
      nm [xml|<p class="h-blah"><img alt="DOING"><img alt="IT"><img alt="WRONG">Goodbye Img!</p>|] `shouldBe` pure "Goodbye Img!"
      nm [xml|<p class="h-blah"><abbr title="Hello Only Abbr!">HOA</abbr></p>|] `shouldBe` pure "Hello Only Abbr!"
      nm [xml|<p class="h-blah"><abbr title="DOING"/><abbr title="IT"/><abbr title="WRONG"/>Goodbye Abbr!</p>|] `shouldBe` pure "Goodbye Abbr!"
      nm [xml|<p class="h-blah"><em><img alt="Hello Only Nested Img!"></p>|] `shouldBe` pure "Hello Only Nested Img!"
      nm [xml|<p class="h-blah"><em><img alt="DOING"><img alt="WRONG">Goodbye Nested Img!</p>|] `shouldBe` pure "Goodbye Nested Img!"
      nm [xml|<p class="h-blah"><em><abbr title="Hello Only Nested Abbr!">HOA</abbr></p>|] `shouldBe` pure "Hello Only Nested Abbr!"
      nm [xml|<p class="h-blah"><em><abbr title="DOING"/><abbr title="WRONG"/>Goodbye Nested Abbr!</p>|] `shouldBe` pure "Goodbye Nested Abbr!"
      nm [xml|<p class="h-blah">Hello Text!</p>|] `shouldBe` pure "Hello Text!"

    it "parses implied u-photo" $ do
      let ph = implyProperty U "photo" . documentRoot . parseLBS
      ph [xml|<img class="h-blah" src="selfie.png">|] `shouldBe` pure "selfie.png"
      ph [xml|<object class="h-blah" data="art.svg">|] `shouldBe` pure "art.svg"
      ph [xml|<div class="h-blah"><img src="selfie.png"/><em>yo</em>|] `shouldBe` pure "selfie.png"
      ph [xml|<div class="h-blah"><object data="art.svg"/><em>yo</em>|] `shouldBe` pure "art.svg"
      ph [xml|<div class="h-blah"><p class="onlychild"><img src="selfie.png"/><em>yo</em>|] `shouldBe` pure "selfie.png"
      ph [xml|<div class="h-blah"><p class="onlychild"><object data="art.svg"/><em>yo</em>|] `shouldBe` pure "art.svg"

    it "parses implied u-url" $ do
      let ur = implyProperty U "url" . documentRoot . parseLBS
      ur [xml|<a class="h-blah" href="/hello/a">|] `shouldBe` pure "/hello/a"
      ur [xml|<area class="h-blah" href="/hello/area">|] `shouldBe` pure "/hello/area"
      ur [xml|<div class="h-blah"><em>what</em><a href="/hello/n/a">|] `shouldBe` pure "/hello/n/a"
      ur [xml|<div class="h-blah"><em>what</em><area href="/hello/n/area">|] `shouldBe` pure "/hello/n/area"
      ur [xml|<div class="h-blah"><div><a href="/nope"></div>|] `shouldBe` Nothing
      ur [xml|<div class="h-blah"><a href="/nope" class="h-nope">n0pe</a>|] `shouldBe` Nothing
