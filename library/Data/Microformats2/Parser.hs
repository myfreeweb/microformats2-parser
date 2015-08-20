{-# LANGUAGE UnicodeSyntax, OverloadedStrings, CPP #-}

module Data.Microformats2.Parser (
  Mf2ParserSettings (..)
, HtmlContentMode (..)
, parseMf2

  -- * HTML parsing stuff (from html-conduit, xml-lens)
, documentRoot
, parseLBS
, sinkDoc
) where

import           Text.HTML.DOM
import           Text.XML.Lens hiding ((.=))
#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import           Data.Microformats2.Parser.Property
import           Data.Microformats2.Parser.HtmlUtil
import           Data.Microformats2.Parser.Util
import           Data.Default
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Lens
import           Data.Char (isSpace)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import           Data.Maybe
import qualified Data.Text as T
import           Safe (headMay)

data Mf2ParserSettings = Mf2ParserSettings 
  { htmlMode ∷ HtmlContentMode }
-- TODO: URL base and resolution

instance Default Mf2ParserSettings where
  def = Mf2ParserSettings { htmlMode = Sanitize }

mf2Elements ∷ Traversal' Element Element
mf2Elements = attributeSatisfies "class" $ any isMf2Class . T.split isSpace

readPropertyName ∷ T.Text → (T.Text, T.Text)
readPropertyName x = (fromMaybe "p" $ headMay ps, T.intercalate "-" $ drop 1 ps)
  where ps = T.splitOn "-" x

extractProperty ∷ HtmlContentMode → T.Text → Element → Value
extractProperty _ "p"  e = fromMaybe Null $ String <$> extractP e
extractProperty _ "u"  e = fromMaybe Null $ String <$> extractU e
extractProperty _ "dt" e = fromMaybe Null $ String <$> extractDt e
extractProperty m "e"  e = object [ "html" .= getProcessedInnerHtml m e, "value" .= getInnerTextRaw e ]
extractProperty _ _    _ = Null

-- lens-aeson's 'key' doesn't add new keys :-(

addValue ∷ T.Text → Value → Value → Value
addValue "p" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "name" . nth 0) o
addValue "e" (Object o)   f = Object $ HMS.insert "value" (fromMaybe Null $ f ^? key "value") $ HMS.insert "html" (fromMaybe Null $ f ^? key "html") $ o
addValue "u" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "url" . nth 0) o
addValue _   (Object o)   f = Object $ HMS.insert "value" f o
addValue _   x            _ = x

addImpliedProperties ∷ Element → Value → Value
addImpliedProperties e v@(Object o) = Object $ addIfNull "photo" "photo" $ addIfNull "url" "url" $ addIfNull "name"  "name" o
  where addIfNull nameJ nameH obj = if null $ v ^? key nameJ then HMS.insert nameJ (singleton $ implyProperty nameH e) obj else obj
        singleton x = fromMaybe Null $ (Array . V.singleton . String) <$> x
addImpliedProperties _ v = v

removePropertiesOfNestedMicroformats ∷ [Element] → [Element] → [Element]
removePropertiesOfNestedMicroformats nmf2s es = filter (not . isNested) es
    where isNested e = any (\e' → e `elem` (filter (/= e') $ e' ^.. entire)) nmf2s

parseProperty ∷ Mf2ParserSettings → Element → [Pair]
parseProperty settings e =
  let propNames = groupBy' snd $ map readPropertyName $ filter isPropertyClass $ classes e
      extractPropertyValue (t, _) = extractProperty (htmlMode settings) t e in
  if any isMf2Class $ classes e
     then map (\(n, ts) → n .= [ addValue (fst $ head ts) (parseH settings e) (extractPropertyValue $ head ts) ]) propNames
     else map (\(n, ts) → n .= map extractPropertyValue ts) propNames

parseH ∷ Mf2ParserSettings → Element → Value
parseH settings e =
  object $ filter (not . emptyVal . snd) [
    "type" .= typeNames, "properties" .= properties, "children" .= childrenMf2 ]
  where typeNames = filter isMf2Class $ classes e
        childrenMf2 = map (\x → addValue "p" x Null) $ map (parseH settings) $ filter (not . isProperty) $ deduplicateElements $ allMf2Descendants 
        allMf2Descendants = filter (/= e) $ e ^.. entire . mf2Elements
        -- we have to do all of this because multiple elements can become multiple properties (with overlap)
        properties = Object $ HMS.filter (not . emptyVal) $ properties'
        (Object properties') = addImpliedProperties e $ object $ map mergeProps $ groupBy' fst properties''
        properties'' = concat $ map (parseProperty settings) $ removePropertiesOfNestedMicroformats allMf2Descendants $ filter (/= e) $ e ^.. entire . propertyElements
        mergeProps (n, vs) = (n, Array $ V.concat $ reverse $ map (extractVector . snd) vs)
        extractVector (Array v) = v
        extractVector _ = V.empty

-- | Parses Microformats 2 from an HTML Element into a JSON Value.
parseMf2 ∷ Mf2ParserSettings → Element → Value
parseMf2 settings rootEl = object [ "items" .= items, "rels" .= rels, "rel-urls" .= relUrls ]
  where items = map (parseH settings) $ deduplicateElements $ rootEl ^.. entire . mf2Elements
        rels = object $ map (\(r, es) → r .= map snd es) $ groupBy' fst $ expandSnd $ map (\e → ((T.split isSpace $ e ^. attr "rel"), e ^. attr "href")) linkEls
        relUrls = object $ map relUrlObject linkEls
        relUrlObject e = (e ^. attr "href") .= object (filter (not . emptyVal . snd) [
            "rels" .= (T.split isSpace $ e ^. attr "rel")
          , "text" .= fromMaybe Null (String <$> getInnerTextWithImgs e)
          , linkAttr "type" "type" e
          , linkAttr "media" "media" e
          , linkAttr "hreflang" "hreflang" e ])
        linkAttr nameJ nameH e = nameJ .= fromMaybe Null (String <$> e ^. attribute nameH)
        linkEls = filter (not . null . (^. attribute "href")) $ filter (not . null . (^. attribute "rel")) $ rootEl ^.. entire . els [ "a", "link" ]
