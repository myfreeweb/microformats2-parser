{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec
import           TestCommon
import           Data.Microformats2.Parser

spec ∷ Spec
spec = do
  describe "parseItems" $ do
    let parseMf2' = parseMf2 def . documentRoot . parseLBS
    it "parses items" $ do
      parseMf2' [xml|<body>
  <div class="h-something aaa h-something-else">
    <h1 class="someclass p-name eeee">Name</h1>
    <header><a class="p-name u-url" href="http://main.url">other name</a></header>
    <span class="aaaaaap-nothingaaaa">---</span>
    <section class="p-org h-card">
      <a class="p-name">Card</a>
    </section>
    <a href="http://card.url" class="p-org h-card">org</a>
    <time class="dt-published p-published dt-updated" datetime="2015-07-17T21:05:13+00:00">17<sup>th</sup> of July 2015 at 21:05</time>
    <div class="h-nested-something">
      haz name
    </div>
  </div>
</body>|] `shouldBe` [json|{
    "items": [
        {
            "type": [ "h-something", "h-something-else" ],
            "properties": {
                "org": [
                    {
                        "type": [ "h-card" ],
                        "properties": {
                            "name": [ "Card" ]
                        },
                        "value": "Card"
                    },
                    {
                        "type": [ "h-card" ],
                        "properties": {
                            "name": [ "org" ],
                            "url": [ "http:\/\/card.url" ]
                        },
                        "value": "org"
                    }
                ],
                "url": [ "http:\/\/main.url" ],
                "name": [ "Name", "other name" ],
                "published": [ "17th of July 2015 at 21:05", "2015-07-17T21:05:13+00:00" ],
                "updated": [ "2015-07-17T21:05:13+00:00" ]
            },
            "children": [
                {
                    "type": [ "h-nested-something" ],
                    "properties": {
                        "name": [ "haz name" ]
                    },
                    "value": "haz name"
                }
            ]
        }
    ],
    "rels": {}
}|]

    it "inserts value and html into e-* h-* properties" $ do
      parseMf2' [xml|<body class=h-parent><div class="h-child e-prop">some <abbr>html</abbr> and <span class="p-name">props|] `shouldBe` [json|{
    "items": [
        {
            "type": [ "h-parent" ],
            "properties": {
                "name": [ "some html and props" ],
                "prop": [
                    {
                        "type": [ "h-child" ],
                        "properties": {
                            "name": [ "props" ]
                        },
                        "value": "some html and props",
                        "html": "some <abbr>html</abbr> and <span class=\"p-name\">props</span>"
                    }
                ]
            }
        }
    ],
    "rels": {}
}|]

    it "parses nested properties but doesn't parse properties of nested microformats into the parent" $ do
      parseMf2' [xml|<body class=h-parent>
                <span class="p-outer"><span class="p-inner">some</span>thing</span>
                <div class="p-outer"><div class="h-child p-prop"><span class="p-aaa">a</span></div></div>
                <div class="h-child"><span class="p-bbb">b</span></div>|] `shouldBe` [json|{
    "items": [
        {
            "type": [ "h-parent" ],
            "properties": {
                "name": [ "something a b" ],
                "outer": [ "something", "a" ],
                "inner": [ "some" ],
                "prop": [ {
                    "type": [ "h-child" ],
                    "properties": {
                        "name": [ "a" ],
                        "aaa": [ "a" ]
                    },
                    "value": "a"
                } ]
            },
            "children": [ {
                "type": [ "h-child" ],
                "properties": {
                    "name": [ "b" ],
                    "bbb": [ "b" ]
                },
                "value": "b"
            } ]
        }
    ],
    "rels": {}
}|]
