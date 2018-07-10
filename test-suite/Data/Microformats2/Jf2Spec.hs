{-# LANGUAGE NoImplicitPrelude, QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.Microformats2.Jf2Spec (spec) where

import           Prelude.Compat
import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Data.Microformats2.Jf2

spec ∷ Spec
spec = do
  describe "mf2ToJf2" $ do
    it "converts microformats2 to JF2" $ do
      mf2ToJf2 [json|{}|] `shouldBe` [json|{}|]
      mf2ToJf2 [json|[{"type": ["h-meme", "lol"]}]|] `shouldBe` [json|[{"type": "meme"}]|]
      mf2ToJf2 [json|{"type": ["h-meme", "lol"]}|] `shouldBe` [json|{"type": "meme"}|]
      mf2ToJf2 [json|{"type": []}|] `shouldBe` [json|{"type": null}|]
      mf2ToJf2 [json|{"type": ["h-ello"], "properties": {"category": ["Dank", "Memes"]}}|]
        `shouldBe` [json|{"type": "ello", "category": ["Dank", "Memes"]}|]
      mf2ToJf2 [json|{"type": ["h-ello"], "properties": {"category": ["Memes"]}}|]
        `shouldBe` [json|{"type": "ello", "category": "Memes"}|]
      mf2ToJf2 [json|{"type": ["h-entry"], "properties": {"category": ["Memes"], "author": [{"type": ["h-card"], "properties": {"name": ["Alice"]}}]}}|]
        `shouldBe` [json|{"type": "entry", "category": "Memes", "author": {"type": "card", "name": "Alice"}}|]
      mf2ToJf2 [json|{"type": ["h-entry"], "properties": {"category": ["Memes"], "author": ["Bob", {"type": ["h-card"], "properties": {"name": ["Alice"]}}]}}|]
        `shouldBe` [json|{"type": "entry", "category": "Memes", "author": ["Bob", {"type": "card", "name": "Alice"}]}|]
      mf2ToJf2 [json|{"type": ["h-entry"], "children": [{"type": ["h-card"], "properties": {"name": ["Alice"]}}]}|]
        `shouldBe` [json|{"type": "entry", "children": [{"type": "card", "name": "Alice"}]}|]
      mf2ToJf2 [json|{ "items":
        [
          {
            "type": [ "h-entry" ],
            "properties": {
              "author": [
                {
                  "type": [ "h-card" ],
                  "properties": {
                    "name": [ "A. Developer" ],
                    "url": [ "http://example.com" ]
                  },
                  "value": "A. Developer"
                }
              ],
              "name": [ "Hello World" ],
              "summary": [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus imperdiet ultrices pulvinar." ],
              "url": [ "http://example.com/2015/10/21" ],
              "published": [ "2015-10-21T12:00:00-0700" ],
              "content": [
                {
                  "html": "<p>Donec dapibus enim lacus, <i>a vehicula magna bibendum non</i>. Phasellus id lacinia felis, vitae pellentesque enim. Sed at quam dui. Suspendisse accumsan, est id pulvinar consequat, urna ex tincidunt enim, nec sodales lectus nulla et augue. Cras venenatis vehicula molestie. Donec sagittis elit orci, sit amet egestas ex pharetra in.</p>",
                  "value": "Donec dapibus enim lacus, a vehicula magna bibendum non. Phasellus id lacinia felis, vitae pellentesque enim. Sed at quam dui. Suspendisse accumsan, est id pulvinar consequat, urna ex tincidunt enim, nec sodales lectus nulla et augue. Cras venenatis vehicula molestie. Donec sagittis elit orci, sit amet egestas ex pharetra in."
                }
              ]
            }
          }
        ]
      }|] `shouldBe` [json|{
        "type": "entry",
        "author": {
          "type": "card",
          "url": "http://example.com",
          "name": "A. Developer"
        },
        "url": "http://example.com/2015/10/21",
        "published": "2015-10-21T12:00:00-0700",
        "name": "Hello World",
        "summary": "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus imperdiet ultrices pulvinar.",
        "content": {
          "html": "<p>Donec dapibus enim lacus, <i>a vehicula magna bibendum non</i>. Phasellus id lacinia felis, vitae pellentesque enim. Sed at quam dui. Suspendisse accumsan, est id pulvinar consequat, urna ex tincidunt enim, nec sodales lectus nulla et augue. Cras venenatis vehicula molestie. Donec sagittis elit orci, sit amet egestas ex pharetra in.</p>",
          "text": "Donec dapibus enim lacus, a vehicula magna bibendum non. Phasellus id lacinia felis, vitae pellentesque enim. Sed at quam dui. Suspendisse accumsan, est id pulvinar consequat, urna ex tincidunt enim, nec sodales lectus nulla et augue. Cras venenatis vehicula molestie. Donec sagittis elit orci, sit amet egestas ex pharetra in."
        }
      }|]
