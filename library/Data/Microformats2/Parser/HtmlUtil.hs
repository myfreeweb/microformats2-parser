{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, RankNTypes  #-}

module Data.Microformats2.Parser.HtmlUtil (
  HtmlContentMode (..)
, getInnerHtml
, getInnerHtmlSanitized
, getInnerTextRaw
, getInnerTextWithImgs
, getProcessedInnerHtml
, deduplicateElements
) where

import           Prelude.Compat
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Foldable (asum)
import           Data.Maybe
import           Text.Blaze
import           Text.Blaze.Renderer.Text
import           Text.HTML.SanitizeXSS
import           Text.XML.Lens hiding (re)
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

renderInner ∷ Element → Text
renderInner = T.concat . map renderNode . elementNodes
  where renderNode (NodeContent c) = c
        renderNode (NodeElement e) = TL.toStrict $ renderMarkup $ toMarkup e
        renderNode _ = ""

getInnerHtml ∷ Maybe URI → Element → Maybe Text
getInnerHtml b rootEl = Just $ renderInner processedRoot
  where (NodeElement processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent $ collapseWhitespace c
        processNode (NodeElement e) = NodeElement $ processChildren processNode $ resolveHrefSrc b e
        processNode x = x

sanitizeAttrs ∷ Element → Element
sanitizeAttrs e = e { elementAttributes = M.fromList $ map wrapName $ mapMaybe modify $ M.toList $ elementAttributes e }
  where modify (Name n _ _, val) = sanitizeAttribute (n, val)
        wrapName (n, val) = (Name n Nothing Nothing, val)

getInnerHtmlSanitized ∷ Maybe URI → Element → Maybe Text
getInnerHtmlSanitized b rootEl = Just $ renderInner processedRoot
  where (NodeElement processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent $ collapseWhitespace $ escapeHtml c
        processNode (NodeElement e) = NodeElement $ processChildren processNode $ filterChildElements (safeTagName . nameLocalName . elementName) $ resolveHrefSrc b $ sanitizeAttrs e
        processNode x = x

getInnerTextRaw ∷ Element → Maybe Text
getInnerTextRaw rootEl = unless' (txt == Just "") txt
  where txt = Just $ T.dropAround (== ' ') $ collapseWhitespace processedRoot
        (NodeContent processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent $ collapseWhitespace $ escapeHtml c
        processNode (NodeElement e) = NodeContent $ collapseWhitespace $ renderInner $ processChildren processNode $ filterChildElements (safeTagName . nameLocalName . elementName) e
        processNode x = x

getInnerTextWithImgs ∷ Element → Maybe Text
getInnerTextWithImgs rootEl = unless' (txt == Just "") txt
  where txt = Just $ T.dropAround (== ' ') $ collapseWhitespace processedRoot
        (NodeContent processedRoot) = processNode (NodeElement rootEl)
        processNode (NodeContent c) = NodeContent $ collapseWhitespace $ escapeHtml c
        processNode (NodeElement e) | nameLocalName (elementName e) == "img" = NodeContent $ fromMaybe "" $ asum [ e ^. attribute "alt", e ^. attribute "src" ]
        processNode (NodeElement e) = NodeContent $ collapseWhitespace $ renderInner $ processChildren processNode $ filterChildElements (safeTagName . nameLocalName . elementName) e
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
