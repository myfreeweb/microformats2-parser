{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Network.URI (parseURI)
import           Data.Microformats2.Parser

spec ∷ Spec
spec = do
  describe "parseItems" $ do
    let parseMf2'' c = parseMf2 c . documentRoot . parseLBS
    let parseMf2' = parseMf2'' def
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
    "rels": {},
    "rel-urls": {}

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
    "rels": {},
    "rel-urls": {}
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
    "rels": {},
    "rel-urls": {}
}|]

    it "parses rels" $ do
      parseMf2' [xml|<html>
  <head><link rel="alternate feed me" href="/atom.xml" type="application/atom+xml"></head>
  <body>
    <a href="//example.com">test</a>
    <a href="https://twitter.com/myfreeweb" rel=me>twitter</a>
    <p><span><a href="https://github.com/myfreeweb" rel=me media=handheld hreflang=en>github</a></span></p>
    <footer><a href="/-1" rel="prev">-1</a></footer>
  </body>
</html>|] `shouldBe` [json|{
    "items": [],
    "rels": {
        "feed": [ "/atom.xml" ],
        "me": [ "https:\/\/github.com\/myfreeweb", "https:\/\/twitter.com\/myfreeweb", "/atom.xml" ],
        "alternate": [ "/atom.xml" ],
        "prev": [ "/-1" ]
    },
    "rel-urls": {
        "/atom.xml": { 
            "type": "application/atom+xml",
            "rels": [ "alternate", "feed", "me" ]
        },
        "https://twitter.com/myfreeweb": { 
            "text": "twitter",
            "rels": [ "me" ]
        },
        "https://github.com/myfreeweb": { 
            "text": "github",
            "rels": [ "me" ],
            "media": "handheld",
            "hreflang": "en"
        },
        "/-1": {
            "text": "-1",
            "rels": [ "prev" ]
        }
    }
}|]

    it "resolves relative URIs" $ do

      parseMf2' [xml|<html> <base href="http://example.com"> <a href="/atom.xml" rel=me>feed</a> </html>|] `shouldBe` [json|{
    "items": [],
    "rels": { "me": [ "http://example.com/atom.xml" ] },
    "rel-urls": { "http://example.com/atom.xml": { "text": "feed", "rels": [ "me" ] } } }|]

      parseMf2'' (def { baseUri = parseURI "http://com.example" }) [xml|<html> <a href="/atom.xml" rel=me>feed</a> </html>|] `shouldBe` [json|{
    "items": [],
    "rels": { "me": [ "http://com.example/atom.xml" ] },
    "rel-urls": { "http://com.example/atom.xml": { "text": "feed", "rels": [ "me" ] } } }|]

      parseMf2'' (def { baseUri = parseURI "http://com.example" }) [xml|<html> <base href="http://example.com"> <a href="/atom.xml" rel=me>feed</a> </html>|] `shouldBe` [json|{
    "items": [],
    "rels": { "me": [ "http://example.com/atom.xml" ] },
    "rel-urls": { "http://example.com/atom.xml": { "text": "feed", "rels": [ "me" ] } } }|]

      parseMf2'' (def { baseUri = parseURI "http://com.example" }) [xml|<html> <base href="/base/"> <a href="atom.xml" rel=me>feed</a> </html>|] `shouldBe` [json|{
    "items": [],
    "rels": { "me": [ "http://com.example/base/atom.xml" ] },
    "rel-urls": { "http://com.example/base/atom.xml": { "text": "feed", "rels": [ "me" ] } } }|]

      parseMf2' [xml|<html> <base href="/base/"> <a href="atom.xml" rel=me>feed</a> </html>|] `shouldBe` [json|{
    "items": [],
    "rels": { "me": [ "/base/atom.xml" ] },
    "rel-urls": { "/base/atom.xml": { "text": "feed", "rels": [ "me" ] } } }|]

      parseMf2'' (def { baseUri = parseURI "http://com.example" }) [xml|<html>
  <base href="/base/">
  <a href="atom.xml" rel=me>feed</a>
  <div class=h-card>
    <h1 class=p-name>card</h1>
    <a href="url" class=u-url>url</a>
    <span href="url" class=u-url><span class=value>/not</span>!!!<em class=value>/resolved</em></span>
    <img src="photo.webp" alt="photo of me"> <!-- implied by :only-of-type -->
  </div>
</html>|] `shouldBe` [json|{
    "items": [
        {
            "type": [ "h-card" ],
            "properties": {
                "photo": [ "http://com.example/base/photo.webp" ],
                "url": [ "http://com.example/base/url", "/not/resolved" ],
                "name": [ "card" ]
            }
        }
    ],
    "rels": { "me": [ "http://com.example/base/atom.xml" ] },
    "rel-urls": { "http://com.example/base/atom.xml": { "text": "feed", "rels": [ "me" ] } }
}|]
