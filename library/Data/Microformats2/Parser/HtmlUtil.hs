{-# LANGUAGE Safe, NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, RankNTypes #-}

module Data.Microformats2.Parser.HtmlUtil (
  HtmlContentMode (..)
, getInnerHtml
, getInnerHtmlSanitized
, getInnerTextRaw
, getInnerTextWithImgs
, getProcessedInnerHtml
, deduplicateElements
, unescapeHtml
) where

import           Prelude.Compat
import           Control.Applicative ((<|>))
import           Control.Error.Util (hush)
import           Data.Monoid.Compat
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isSpace)
import           Data.Foldable (asum)
import           Data.Attoparsec.Text
import           Data.Maybe
import           Text.HTML.SanitizeXSS
import           Text.HTML.TagSoup.Entity
import           Network.URI
import           Data.Microformats2.Parser.Util

resolveHrefSrc ∷ Maybe URI → Element → Element
resolveHrefSrc b e = e { elementAttributes = M.adjust (resolveURI b) "href" $ M.adjust (resolveURI b) "href" $ elementAttributes e }

processChildren ∷ (Node → Node) → Element → Element
processChildren f e = e { elementNodes = map f $ elementNodes e }

filterChildElements ∷ (Element → Bool) → Element → Element
filterChildElements f e = e { elementNodes = filter f' $ elementNodes e }
  where f' (NodeContent _) = True
        f' (NodeElement e') = f e'
        f' _ = False

getInnerHtml ∷ Maybe URI → Element → Maybe Text
getInnerHtml b rootEl = Just $ renderInner processedRoot
  where (NodeElement processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent c
        processNode (NodeElement e) = NodeElement $ processChildren processNode $ resolveHrefSrc b e
        processNode x = x

sanitizeAttrs ∷ Element → Element
sanitizeAttrs e = e { elementAttributes = M.fromList $ map wrapName $ mapMaybe modify $ M.toList $ elementAttributes e }
  where modify (Name n _ _, val) = sanitizeAttribute (n, val)
        wrapName (n, val) = (Name n Nothing Nothing, val)

getInnerHtmlSanitized ∷ Maybe URI → Element → Maybe Text
getInnerHtmlSanitized b rootEl = Just $ renderInner processedRoot
  where (NodeElement processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent c
        processNode (NodeElement e) = NodeElement $ processChildren processNode $ filterChildElements (safeTagName' . nameLocalName . elementName) $ resolveHrefSrc b $ sanitizeAttrs e
        processNode x = x

getInnerTextRaw ∷ Element → Maybe Text
getInnerTextRaw rootEl = unless' (txt == Just "") txt
  where txt = Just $ T.dropAround isSpace processedRoot
        (NodeContent processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent $ escapeHtml c
        processNode (NodeElement e) = NodeContent $ T.dropAround isSpace $ renderInner $ processChildren processNode $ filterChildElements (safeTagName' . nameLocalName . elementName) e
        processNode x = x

getInnerTextWithImgs ∷ Element → Maybe Text
getInnerTextWithImgs rootEl = unless' (txt == Just "") txt
  where txt = Just $ T.dropAround isSpace processedRoot
        (NodeContent processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent $ escapeHtml c
        processNode (NodeElement e) | nameLocalName (elementName e) == "img" = NodeContent $ fromMaybe "" $ asum [ e ^. attribute "alt", e ^. attribute "src" ]
        processNode (NodeElement e) = NodeContent $ T.dropAround isSpace $ renderInner $ processChildren processNode $ filterChildElements (safeTagName' . nameLocalName . elementName) e
        processNode x = x

data HtmlContentMode = Unsafe | Escape | Sanitize
  deriving (Show, Eq)

getProcessedInnerHtml ∷ HtmlContentMode → Maybe URI → Element → Maybe Text
getProcessedInnerHtml Unsafe   b e = getInnerHtml b e
getProcessedInnerHtml Escape   b e = escapeHtml <$> getInnerHtml b e
getProcessedInnerHtml Sanitize b e = getInnerHtmlSanitized b e

deduplicateElements ∷ [Element] → [Element]
deduplicateElements es = filter (not . isNested) es
  where isNested e = any (\e' → e `elem` filter (/= e') (e' ^.. entire)) es
        -- not the fastest function I guess...

escapeHtml ∷ Text → Text
escapeHtml = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

unescapeHtml ∷ Text → Text
unescapeHtml x = fromMaybe x $ T.concat <$> hush (parseOnly p x)
  where p = many1 $ takeWhile1 (/= '&') <|> pent 
        pent = do
          _ ← char '&'
          ent ← takeWhile1 (/= ';')
          _ ← char ';'
          return $ fromMaybe ("&" <> ent <> ";") $ T.pack <$> lookupEntity (T.unpack ent)

safeTagName' ∷ Text → Bool
safeTagName' x = safeTagName x || "-" `T.isInfixOf` x
