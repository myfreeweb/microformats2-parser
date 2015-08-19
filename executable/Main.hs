{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Main (main) where

import           Control.Exception
import           Data.Microformats2.Parser
import           Data.List
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import           Data.Streaming.Network (bindPath)
import qualified Data.Stringable as S
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Socket as S
import           Web.Scotty
import           Text.Blaze.Html5 as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Options as O


data AppOptions = AppOptions
  { port                     ∷ Int
  , socket                   ∷ String
  , protocol                 ∷ String }

instance O.Options AppOptions where
  defineOptions = pure AppOptions
    <*> O.simpleOption "port"              3000                                  "The port the app should listen for connections on (for http protocol)"
    <*> O.simpleOption "socket"            "/var/run/mf2/mf2.sock"               "The UNIX domain socket the app should listen for connections on (for unix protocol)"
    <*> O.simpleOption "protocol"          "http"                                "The protocol for the server. One of: http, unix, cgi"

exampleValue = "<body> <p class='h-adr'>   <span class='p-street-address'>17 Austerstræti</span>   <span class='p-locality'>Reykjavík</span>   <span class='p-country-name'>Iceland</span>   <span class='p-postal-code'>107</span> </p> <div class='h-card'>   <a class='p-name u-url'      href='http://blog.lizardwrangler.com/'      >Mitchell Baker</a>    (<a class='p-org h-card'        href='http://mozilla.org/'      >Mozilla Foundation</a>) </div> <article class='h-entry'>   <h1 class='p-name'>Microformats are amazing</h1>   <p>Published by <a class='p-author h-card' href='http://example.com'>W. Developer</a>      on <time class='dt-published' datetime='2013-06-13 12:00:00'>13<sup>th</sup> June 2013</time>     <p class='p-summary'>In which I extoll the virtues of using microformats.</p>     <div class='e-content'>     <p>Blah blah blah</p>   </div> </article> <span class='h-cite'>   <time class='dt-published'>YYYY-MM-DD</time>    <span class='p-author h-card'>AUTHOR</span>:    <cite><a class='u-url p-name' href='URL'>TITLE</a></cite> </span> </body>"

homePage ∷ TL.Text → Html → Html
homePage v result = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "utf-8"
    H.title "microformats2-parser"
    H.style "body { font-family: 'Helvetica Neue', sans-serif; max-width: 900px; margin: 0 auto; } a { color: #ba2323; } a:hover { color: #da4343; } pre, textarea, button { width: 100%; } textarea { resize: vertical; min-height: 10em; margin-bottom: 1em; } pre { white-space: pre-wrap; } footer { margin: 2em 0; }"
  H.body $ do
    h1 $ do
      a ! href "https://github.com/myfreeweb/microformats2-parser" $ "microformats2-parser"
    p "This is a test page for the Microformats 2 Haskell parser."
    p "Note:"
    ul $ do
      li "this demo page uses the Sanitize mode for e-content in h-cite and h-entry"
    H.form ! method "post" ! action "" $ do
      textarea ! name "html" $ toHtml v
      button "Parse!"
    result
    footer $ do
      a ! href "https://unrelenting.technology" $ "unrelenting.technology"

display ∷ Show α ⇒ α → Html
display = pre . toHtml . S.toLazyText . show

parseResult ∷ TL.Text → Html
parseResult h = section $ do
  let root = documentRoot $ parseLBS $ S.toLazyByteString h
  h2 "h-geo"
  display $ parseGeo root
  h2 "h-adr"
  display $ parseAdr root
  h2 "h-card"
  display $ parseCard root
  h2 "h-cite"
  display $ parseCite Sanitize root
  h2 "h-entry"
  display $ parseEntry Sanitize root

app = scottyApp $ do
  matchAny "/" $ do
    ps ← params
    let h' = lookup "html" ps
        result = case h' of
                   Just h → parseResult h
                   Nothing → toHtml ("" ∷ TL.Text)
    setHeader "Content-Type" "text/html; charset=utf-8"
    raw $ renderHtml $ homePage (fromMaybe exampleValue h') result

main = O.runCommand $ \opts args → do
  let warpSettings = setPort (port opts) defaultSettings
  case protocol opts of
    "http" → app >>= runSettings warpSettings
    "unix" → bracket (bindPath $ socket opts) S.close (\socket → app >>= runSettingsSocket warpSettings socket)
    "cgi" → app >>= CGI.run
    _ → putStrLn $ "Unsupported protocol: " ++ protocol opts
