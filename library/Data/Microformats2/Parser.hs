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
import           Network.URI
import           Safe (headMay)

data Mf2ParserSettings = Mf2ParserSettings
  { htmlMode ∷ HtmlContentMode
  , baseUri  ∷ Maybe URI }
  deriving (Show, Eq)

instance Default Mf2ParserSettings where
  def = Mf2ParserSettings { htmlMode = Sanitize
                          , baseUri  = Nothing }

mf2Elements ∷ Traversal' Element Element
mf2Elements = attributeSatisfies "class" $ any isMf2Class . T.split isSpace

readPropertyName ∷ T.Text → (T.Text, T.Text)
readPropertyName x = (fromMaybe "p" $ headMay ps, T.intercalate "-" $ drop 1 ps)
  where ps = T.splitOn "-" x

extractProperty ∷ Mf2ParserSettings → T.Text → Element → Value
extractProperty _ "p"  e = fromMaybe Null $ String <$> extractP e
extractProperty s "u"  e = fromMaybe Null $ String <$> resolveUrl s <$> extractU e -- XXX: spec says only resolve when href/src/data but not value-class/value/title/innerText -- WTF?
extractProperty _ "dt" e = fromMaybe Null $ String <$> extractDt e
extractProperty s "e"  e = object [ "html" .= getProcessedInnerHtml (htmlMode s) e, "value" .= getInnerTextRaw e ]
extractProperty _ _    _ = Null

-- lens-aeson's 'key' doesn't add new keys :-(

addValue ∷ T.Text → Value → Value → Value
addValue "p" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "name" . nth 0) o
addValue "e" (Object o)   f = Object $ HMS.insert "value" (fromMaybe Null $ f ^? key "value") $ HMS.insert "html" (fromMaybe Null $ f ^? key "html") $ o
addValue "u" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "url" . nth 0) o
addValue _   (Object o)   f = Object $ HMS.insert "value" f o
addValue _   x            _ = x

addImpliedProperties ∷ Mf2ParserSettings → Element → Value → Value
addImpliedProperties settings e v@(Object o) = Object $ addIfNull "photo" "photo" resolveUrl' $ addIfNull "url" "url" resolveUrl' $ addIfNull "name" "name" id o
  where addIfNull nameJ nameH f obj = if null $ v ^? key nameJ then HMS.insert nameJ (singleton $ f <$> implyProperty nameH e) obj else obj
        singleton x = fromMaybe Null $ (Array . V.singleton . String) <$> x
        resolveUrl' = resolveUrl settings
addImpliedProperties _ _ v = v

removePropertiesOfNestedMicroformats ∷ [Element] → [Element] → [Element]
removePropertiesOfNestedMicroformats nmf2s es = filter (not . isNested) es
    where isNested e = any (\e' → e `elem` (filter (/= e') $ e' ^.. entire)) nmf2s

parseProperty ∷ Mf2ParserSettings → Element → [Pair]
parseProperty settings e =
  let propNames = groupBy' snd $ map readPropertyName $ filter isPropertyClass $ classes e
      extractPropertyValue (t, _) = extractProperty settings t e in
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
        (Object properties') = addImpliedProperties settings e $ object $ map mergeProps $ groupBy' fst properties''
        properties'' = concat $ map (parseProperty settings) $ removePropertiesOfNestedMicroformats allMf2Descendants $ filter (/= e) $ e ^.. entire . propertyElements
        mergeProps (n, vs) = (n, Array $ V.concat $ reverse $ map (extractVector . snd) vs)
        extractVector (Array v) = v
        extractVector _ = V.empty

-- | Parses Microformats 2 from an HTML Element into a JSON Value.
parseMf2 ∷ Mf2ParserSettings → Element → Value
parseMf2 settings rootEl = object [ "items" .= items, "rels" .= rels, "rel-urls" .= relUrls ]
  where items = map (parseH settings') $ deduplicateElements $ rootEl ^.. entire . mf2Elements
        rels = object $ map (\(r, es) → r .= map snd es) $ groupBy' fst $ expandSnd $ map (\e → ((T.split isSpace $ e ^. attr "rel"), resolveUrl settings' $ e ^. attr "href")) linkEls
        relUrls = object $ map relUrlObject linkEls
        relUrlObject e = (resolveUrl settings' $ e ^. attr "href") .= object (filter (not . emptyVal . snd) [
            "rels" .= (T.split isSpace $ e ^. attr "rel")
          , "text" .= fromMaybe Null (String <$> getInnerTextWithImgs e)
          , linkAttr "type" "type" e
          , linkAttr "media" "media" e
          , linkAttr "hreflang" "hreflang" e ])
        linkAttr nameJ nameH e = nameJ .= fromMaybe Null (String <$> e ^. attribute nameH)
        linkEls = filter (not . null . (^. attribute "href")) $ filter (not . null . (^. attribute "rel")) $ rootEl ^.. entire . els [ "a", "link" ]
        -- Obligatory WTF comment about base[href] being relative to the URI the page was requested from! <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base>
        settings' = settings { baseUri = case (baseUri settings, parseURIReference =<< T.unpack <$> (rootEl ^. entire . el "base" . attribute "href")) of
                                           (Just sU, Just tU) → Just (tU `relativeTo` sU)
                                           (Just sU, Nothing) → Just sU
                                           (Nothing, Just tU) → Just tU
                                           (Nothing, Nothing) → Nothing }

resolveUrl ∷ Mf2ParserSettings → T.Text → T.Text
resolveUrl settings t = case parseURIReference $ T.unpack t of
                          Just u → T.pack $ uriToString id (u `relativeTo` (fromMaybe nullURI $ baseUri settings)) ""
                          Nothing → t
