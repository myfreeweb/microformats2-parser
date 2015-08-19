{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec
import           TestCommon
import           Data.Microformats2.Parser
import           Debug.Trace
import           Data.Aeson hiding (json)

spec ∷ Spec
spec = do
  describe "parseItems" $ do
    let parseMf2' = parseMf2 def . documentRoot . parseLBS
    it "works" $ do
      parseMf2' [xml|<body>
  <div class="h-something aaa h-something-else">
    <h1 class="someclass p-name eeee">Name</h1>
    <span class="p-name">other name</span>
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
