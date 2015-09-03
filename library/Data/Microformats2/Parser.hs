{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, OverloadedStrings #-}

module Data.Microformats2.Parser (
  Mf2ParserSettings (..)
, HtmlContentMode (..)
, parseMf2

  -- * HTML parsing stuff (from html-conduit, xml-lens)
, documentRoot
, parseLBS
, sinkDoc
) where

import           Prelude.Compat
import           Text.HTML.DOM
import           Text.XML.Lens hiding ((.=))
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
extractProperty s "u"  e = fromMaybe Null $ String <$> case extractU e of
                                                         Just (u, True) → Just $ resolveURI (baseUri s) u
                                                         Just (t, False) → Just t
                                                         Nothing → Nothing
extractProperty _ "dt" e = fromMaybe Null $ String <$> extractDt e
extractProperty s "e"  e = object [ "html" .= getProcessedInnerHtml (htmlMode s) (baseUri s) e, "value" .= getInnerTextRaw e ]
extractProperty _ _    _ = Null

-- lens-aeson's 'key' doesn't add new keys :-(

addValue ∷ T.Text → Value → Value → Value
addValue "p" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "name" . nth 0) o
addValue "e" (Object o)   f = Object $ HMS.insert "value" (fromMaybe Null $ f ^? key "value") $ HMS.insert "html" (fromMaybe Null $ f ^? key "html") o
addValue "u" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "url" . nth 0) o
addValue _   (Object o)   f = Object $ HMS.insert "value" f o
addValue _   x            _ = x

addImpliedProperties ∷ Mf2ParserSettings → Element → Value → Value
addImpliedProperties settings e v@(Object o) = Object $ addIfNull "photo" "photo" resolveURI' $ addIfNull "url" "url" resolveURI' $ addIfNull "name" "name" id o
  where addIfNull nameJ nameH f obj = if isNothing $ v ^? key nameJ then HMS.insert nameJ (singleton $ f <$> implyProperty nameH e) obj else obj
        singleton x = fromMaybe Null $ (Array . V.singleton . String) <$> x
        resolveURI' = resolveURI $ baseUri settings
addImpliedProperties _ _ v = v

removePropertiesOfNestedMicroformats ∷ [Element] → [Element] → [Element]
removePropertiesOfNestedMicroformats nmf2s = filter (not . isNested)
    where isNested e = any (\e' → e `elem` filter (/= e') (e' ^.. entire)) nmf2s

parseProperty ∷ Mf2ParserSettings → Element → [Pair]
parseProperty settings e =
  let propNames = groupBy' snd $ map readPropertyName $ filter isPropertyClass $ classes e
      extractPropertyValue (t, _) = extractProperty settings t e in
  if any isMf2Class $ classes e
     then map (\(n, ts) → n .= [ addValue (fst $ head ts) (parseH settings e) (extractPropertyValue $ head ts) ]) propNames
     else map (\(n, ts) → n .= map extractPropertyValue ts) propNames

parseH ∷ Mf2ParserSettings → Element → Value
parseH settings e =
  object $ filter (\(n, v) → not (emptyVal v) || n == "properties") [
      "type"       .= filter isMf2Class (classes e)
    , "properties" .= properties
    , "children"   .= childrenMf2
    , "shape"      .= fromMaybe Null (String <$> e ^? el "area" . attr "shape")
    , "coords"     .= fromMaybe Null (String <$> e ^? el "area" . attr "coords") ]
  where childrenMf2 = map ((\x → addValue "p" x Null) . parseH settings) $ filter (not . isProperty) $ deduplicateElements allMf2Descendants
        allMf2Descendants = filter (/= e) $ e ^.. entire . mf2Elements
        -- we have to do all of this because multiple elements can become multiple properties (with overlap)
        properties = Object $ HMS.filter (not . emptyVal) properties'
        (Object properties') = addImpliedProperties settings e $ object $ map mergeProps $ groupBy' fst properties''
        properties'' = concatMap (parseProperty settings) $ removePropertiesOfNestedMicroformats allMf2Descendants $ filter (/= e) $ e ^.. entire . propertyElements
        mergeProps (n, vs) = (n, Array $ V.concat $ reverse $ map (extractVector . snd) vs)
        extractVector (Array v) = v
        extractVector _ = V.empty

-- | Parses Microformats 2 from an HTML Element into a JSON Value.
parseMf2 ∷ Mf2ParserSettings → Element → Value
parseMf2 settings rootEl = object [ "items" .= items, "rels" .= rels, "rel-urls" .= relUrls ]
  where items = map (parseH settings') $ deduplicateElements $ rootEl' ^.. entire . mf2Elements
        rels = object $ map (\(r, es) → r .= map snd es) $ groupBy' fst $ expandSnd $ map (\e → (T.split isSpace (e ^. attr "rel"), resolveURI (baseUri settings') $ e ^. attr "href")) linkEls
        relUrls = object $ map relUrlObject linkEls
        relUrlObject e = resolveURI (baseUri settings') (e ^. attr "href") .= object (filter (not . emptyVal . snd) [
            "rels" .= T.split isSpace (e ^. attr "rel")
          , "text" .= fromMaybe Null (String <$> getInnerTextWithImgs e)
          , linkAttr "type" "type" e
          , linkAttr "media" "media" e
          , linkAttr "hreflang" "hreflang" e ])
        linkAttr nameJ nameH e = nameJ .= fromMaybe Null (String <$> e ^? attr nameH)
        linkEls = filter (isJust . (^? attr "href")) $ filter (isJust . (^? attr "rel")) $ rootEl' ^.. entire . els [ "a", "link" ]
        -- Obligatory WTF comment about base[href] being relative to the URI the page was requested from! <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base>
        settings' = settings { baseUri = case (baseUri settings, parseURIReference =<< T.unpack <$> (rootEl' ^? entire . el "base" . attr "href")) of
                                           (Just sU, Just tU) → Just (tU `relativeTo` sU)
                                           (Just sU, Nothing) → Just sU
                                           (Nothing, Just tU) → Just tU
                                           (Nothing, Nothing) → Nothing }
        rootEl' = preprocessHtml rootEl

preprocessHtml ∷ Element → Element
preprocessHtml (Element n as ns) = Element (lowerName n) as $ map preprocessChildren $ filter (not . isTemplate) ns
  where isTemplate (NodeElement (Element (Name "template" _ _) _ _)) = True
        isTemplate _ = False
        preprocessChildren (NodeElement e) = NodeElement $ preprocessHtml e
        preprocessChildren x = x
        lowerName (Name txt x y) = Name (T.toLower txt) x y
