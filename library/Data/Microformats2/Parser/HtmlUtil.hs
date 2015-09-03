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

resolveHref ∷ Maybe URI → Element → Element
resolveHref b e = e { elementAttributes = M.adjust (resolveURI b) "href" $ elementAttributes e }

getPrism ∷ Prism' Node Text → Element → Maybe Text
getPrism t e = Just . T.strip <$> T.concat $ e ^.. nodes . traverse . t

_InnerHtml ∷ Maybe URI → Prism' Node Text
_InnerHtml b = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ collapseWhitespace c
  NodeElement e → Just . TL.toStrict . renderMarkup . toMarkup $ resolveHref b e
  _ → Nothing

getInnerHtml ∷ Maybe URI → Element → Maybe Text
getInnerHtml b = getPrism $ _InnerHtml b

sanitizeAttrs ∷ Element → Element
sanitizeAttrs e = e { elementAttributes = M.fromList $ map wrapName $ mapMaybe modify $ M.toList $ elementAttributes e }
  where modify (Name n _ _, val) = sanitizeAttribute (n, val)
        wrapName (n, val) = (Name n Nothing Nothing, val)

_InnerHtmlSanitized ∷ Maybe URI → Prism' Node Text
_InnerHtmlSanitized b = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ collapseWhitespace c
  NodeElement e → if' (safeTagName $ nameLocalName (elementName e)) $
                    Just . TL.toStrict . renderMarkup . toMarkup $ resolveHref b $ sanitizeAttrs e
  _ → Nothing

getInnerHtmlSanitized ∷ Maybe URI → Element → Maybe Text
getInnerHtmlSanitized b = getPrism $ _InnerHtmlSanitized b

_InnerTextRaw ∷ Prism' Node Text
_InnerTextRaw = prism' NodeContent $ \s → case s of
  NodeContent c → Just . collapseWhitespace $ c
  NodeElement e → if' (safeTagName $ nameLocalName (elementName e)) $
                    Just . collapseWhitespace . TL.toStrict . renderMarkup . contents . toMarkup $ e
  _ → Nothing

_InnerTextWithImgs ∷ Prism' Node Text
_InnerTextWithImgs = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ collapseWhitespace c
  NodeElement e → if nameLocalName (elementName e) == "img"
                    then asum [ e ^. attribute "alt", e ^. attribute "src" ]
                    else Just . collapseWhitespace . TL.toStrict . renderMarkup . contents . toMarkup $ e
  _ → Nothing

getInnerTextRaw ∷ Element → Maybe Text
getInnerTextRaw e = unless' (txt == Just "") txt
  where txt = getPrism _InnerTextRaw e

getInnerTextWithImgs ∷ Element → Maybe Text
getInnerTextWithImgs e = unless' (txt == Just "") txt
  where txt = getPrism _InnerTextWithImgs e

data HtmlContentMode = Unsafe | Escape | Sanitize
  deriving (Show, Eq)

getProcessedInnerHtml ∷ HtmlContentMode → Maybe URI → Element → Maybe Text
getProcessedInnerHtml Unsafe   b e = getInnerHtml b e
getProcessedInnerHtml Escape   b e = (T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;") <$> getInnerHtml b e
getProcessedInnerHtml Sanitize b e = getInnerHtmlSanitized b e

deduplicateElements ∷ [Element] → [Element]
deduplicateElements es = filter (not . isNested) es
  where isNested e = any (\e' → e `elem` filter (/= e') (e' ^.. entire)) es
        -- not the fastest function I guess...
