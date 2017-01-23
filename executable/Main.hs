{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

module Main (main) where

import           Prelude.Compat
import           Data.Microformats2.Parser
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types (object)
import           Data.Default
import qualified Data.Text.Lazy as TL
import           Network.Wai.Cli
import           Network.Wai.Middleware.Autohead
import           Network.URI (parseURI)
import           Web.Scotty hiding (html)
import           Text.Blaze.Html5 as H hiding (main, param, object, base)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)


exampleValue = "<body> <p class='h-adr'>   <span class='p-street-address'>17 Austerstræti</span>   <span class='p-locality'>Reykjavík</span>   <span class='p-country-name'>Iceland</span>   <span class='p-postal-code'>107</span> </p> <div class='h-card'>   <a class='p-name u-url'      href='http://blog.lizardwrangler.com/'      >Mitchell Baker</a>    (<a class='p-org h-card'        href='http://mozilla.org/'      >Mozilla Foundation</a>) </div> <article class='h-entry'>   <h1 class='p-name'>Microformats are amazing</h1>   <p>Published by <a class='p-author h-card' href='http://example.com'>W. Developer</a>      on <time class='dt-published' datetime='2013-06-13 12:00:00'>13<sup>th</sup> June 2013</time>     <p class='p-summary'>In which I extoll the virtues of using microformats.</p>     <div class='e-content'>     <p>Blah blah blah</p>   </div> </article> <span class='h-cite'>   <time class='dt-published'>YYYY-MM-DD</time>    <span class='p-author h-card'>AUTHOR</span>:    <cite><a class='u-url p-name' href='URL'>TITLE</a></cite> </span> </body>"

homePage ∷ TL.Text → Html
homePage v = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "utf-8"
    H.title "microformats2-parser"
    H.style "body { font-family: 'Helvetica Neue', sans-serif; max-width: 900px; margin: 0 auto; } a { color: #ba2323; } a:hover { color: #da4343; } pre, input, textarea, button { width: 100%; } input, textarea { margin-bottom: 1em; } textarea { resize: vertical; min-height: 15em; } pre { white-space: pre-wrap; } footer { margin: 2em 0; }"
  H.body $ do
    H.header $ do
      h1 $ do
        a ! href "https://github.com/myfreeweb/microformats2-parser" $ "microformats2-parser"
      a ! href "https://github.com/myfreeweb/microformats2-parser" $ img ! alt "GitHub" ! src "https://img.shields.io/badge/git-hub-gray.svg?style=flat"
      " "
      a ! href "https://hackage.haskell.org/package/microformats2-parser" $ img ! alt "Hackage" ! src "https://img.shields.io/hackage/v/microformats2-parser.svg?style=flat"
      " "
      a ! href "https://travis-ci.org/myfreeweb/microformats2-parser" $ img ! alt "Build Status" ! src "https://img.shields.io/travis/myfreeweb/microformats2-parser.svg?style=flat"
      " "
      a ! href "http://unlicense.org" $ img ! alt "unlicense" ! src "https://img.shields.io/badge/un-license-green.svg?style=flat"
    p "This is a test page for the Microformats 2 Haskell parser."
    p "Notes:"
    ul $ do
      li "this demo page uses the Sanitize mode for e-*"
      li $ do
        a ! href "http://enable-cors.org" $ "CORS is enabled"
        " on the endpoint (POST parse.json, form-urlencoded, 'html' and 'base' parameter)"
      li $ a ! href "http://jsonview.com" $ "JSONView is awesome"
    H.form ! method "post" ! action "parse.json" $ do
      textarea ! name "html" $ toHtml v
      input ! name "base" ! type_ "url" ! placeholder "https://example.com/base/url/for/resolving/relative/urls"
      button "Parse!"
    footer $ do
      a ! href "https://unrelenting.technology" ! rel "author" $ "unrelenting.technology"

app = scottyApp $ do
  middleware autohead

  get "/" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    raw $ renderHtml $ homePage exampleValue

  get "/parse.json" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    json $ object []

  post "/parse.json" $ do
    hsrc ← param "html"
    base ← param "base" `rescue` (\_ → return "")
    setHeader "Content-Type" "application/json; charset=utf-8"
    setHeader "Access-Control-Allow-Origin" "*"
    let root = documentRoot $ parseLBS hsrc
    raw $ encodePretty $ parseMf2 (def { baseUri = parseURI base }) root

main = defWaiMain =<< app
