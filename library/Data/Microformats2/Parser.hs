{-# LANGUAGE UnicodeSyntax, OverloadedStrings, CPP #-}

module Data.Microformats2.Parser (
  module Data.Microformats2.Parser

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
-- import           Control.Monad
-- import           Control.Monad.Trans.Maybe
-- import           Data.Microformats2
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
-- import           Data.Foldable (asum)
import           Data.Maybe
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.ByteString.Lazy as LB
-- import           Network.URI
import           Safe (headMay)

data Mf2ParserSettings = Mf2ParserSettings 
  { htmlMode ∷ HtmlContentMode }

instance Default Mf2ParserSettings where
  def = Mf2ParserSettings { htmlMode = Sanitize }

mf2Elements ∷ Traversal' Element Element
mf2Elements = attributeSatisfies "class" $ any isMf2Class . T.split isSpace

deduplicateMf2 ∷ [Element] → [Element]
deduplicateMf2 es = filter isNested es
        -- XXX: Is there a way to make it NOT O(n^2)? Like, just not going deeper into matching elements in the first place?
  where isNested e = not $ any (\e' → e `elem` e' ^.. plate) es

readPropertyName ∷ T.Text → (T.Text, T.Text)
readPropertyName x = (fromMaybe "p" $ headMay ps, T.intercalate "-" $ drop 1 ps)
  where ps = T.splitOn "-" x

extractProperty ∷ HtmlContentMode → T.Text → Element → Value
extractProperty _ "p"  e = fromMaybe Null $ String <$> extractP e
extractProperty _ "u"  e = fromMaybe Null $ String <$> extractU e
extractProperty _ "dt" e = fromMaybe Null $ String <$> extractDt e
extractProperty m "e"  e = object [ "html" .= getProcessedInnerHtml m e, "value" .= getInnerTextRaw e ]

addValue ∷ T.Text → Value → Value → Value
addValue "p" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "name" . nth 0) o
addValue "e" (Object o)   _ = Object o -- WTF: re-use its { } structure with existing value: inside
addValue "u" v@(Object o) f = Object $ HMS.insert "value" (fromMaybe f $ v ^? key "properties" . key "url" . nth 0) o
addValue _   (Object o)   f = Object $ HMS.insert "value" f o
addValue _   x            _ = x

parseProperty ∷ Mf2ParserSettings → Element → [Pair]
parseProperty settings e =
  let propNames = groupBy' snd $ map readPropertyName $ filter isPropertyClass $ classes e
      extractPropertyValue (t, _) = extractProperty (htmlMode settings) t e in
  if any isMf2Class $ classes e
     then map (\(n, ts) → n .= [ addValue (fst $ head ts) (parseH settings e) (extractPropertyValue $ head ts) ]) propNames
     else map (\(n, ts) → n .= map extractPropertyValue ts) propNames

addImpliedProperties ∷ Element → Value → Value
addImpliedProperties e v@(Object o) =
  let singleton x = fromMaybe Null $ (Array . V.singleton . String) <$> x in
  if null $ v ^? key "name"
     then addImpliedProperties e $ Object $ HMS.insert "name" (singleton $ implyProperty "name" e) o
     else if null $ v ^? key "url"
            then addImpliedProperties e $ Object $ HMS.insert "url" (singleton $ implyProperty "url" e) o
            else if null $ v ^? key "photo"
                   then addImpliedProperties e $ Object $ HMS.insert "photo" (singleton $ implyProperty "photo" e) o
                   else v
addImpliedProperties _ v = v

parseH ∷ Mf2ParserSettings → Element → Value
parseH settings e =
  object $ filter (not . emptyVal . snd) [
    "type" .= typeNames, "properties" .= properties, "children" .= childrenMf2 ]
  where typeNames = filter isMf2Class $ classes e
        properties = Object $ HMS.filter (not . emptyVal) $ properties'
        (Object properties') = addImpliedProperties e $ object $ map mergeProps $ groupBy' fst properties''
        properties'' = concat $ map (parseProperty settings) $ e ^.. plate . propertyElements
        mergeProps (n, vs) = (n, Array $ V.concat $ reverse $ map (extractVector . snd) vs)
        extractVector (Array v) = v
        extractVector _ = V.empty
        childrenMf2 = map (\x → addValue "p" x Null) $ map (parseH settings) $ filter (not . isProperty) $ deduplicateMf2 $ e ^.. plate . mf2Elements

parseMf2 ∷ Mf2ParserSettings → Element → Value
parseMf2 settings rootEl = object [ "items" .= items, "rels" .= object [] ]
  where items = map (parseH settings) $ deduplicateMf2 $ rootEl ^.. entire . mf2Elements

-- -- | Parses the representative h-entry on a URL, discovering its authorship <http://indiewebcamp.com/authorship>, using the HTTP fetcher function from arguments.
-- parseReprEntryWithAuthor ∷ Monad μ ⇒ (URI → μ (Maybe LB.ByteString)) → HtmlContentMode → TL.Text → μ (Maybe Entry)
-- parseReprEntryWithAuthor fetch m href = do
-- -- LOL USE THE MAYBE MONAD REALLY LIKE THE <- THING
--   case parseURI $ TL.unpack href of
--     Nothing → return Nothing
--     Just uri → do
--       entryPage ← fetch uri
--       case entryPage of
--         Nothing → return Nothing
--         Just body → do
--           let rootEl = documentRoot $ parseLBS body
--           case headMay $ parseEntry m rootEl of
--             Nothing → return Nothing
--             Just reprEntry → do
--               discovered ← discoverAuthor fetch rootEl uri reprEntry
--               return $ Just $ reprEntry { entryAuthor = fromMaybe [] discovered }

-- discoverAuthor ∷ Monad μ ⇒ (URI → μ (Maybe LB.ByteString)) → Element → URI → Entry → μ (Maybe [CardReference])
-- discoverAuthor fetch rootEl baseUri entry = do
--   let fetchCard href = do
--         case parseURIReference $ TL.unpack $ TL.strip href of
--           Nothing → return Nothing
--           Just uri → do
--             resp ← fetch $ uri `relativeTo` baseUri
--             case resp of
--               Nothing → return Nothing
--               Just body → return $ CardCard <$> (headMay $ parseCard $ documentRoot $ parseLBS body)
--       fetchAuthorIfUrl (TextCard url) = fetchCard url
--       fetchAuthorIfUrl c = return $ Just c
--       seqListMaybe = liftM (listToMaybeList . catMaybes) . sequence
--       processedOrigAuthors = seqListMaybe $ fetchAuthorIfUrl <$> entryAuthor entry
--       hFeedAuthorCard = do
--         -- TODO: check that the feed actually contains the entry // use stripQueryString
--         case rootEl ^? entire . hasClass "h-feed" of
--           Nothing → return Nothing
--           Just hFeedEl → seqListMaybe $ fetchAuthorIfUrl <$> extractAuthor hFeedEl
--       relAuthorCard = do
--         case rootEl ^. entire . hasRel "author" . attribute "href" of
--           Nothing → return Nothing
--           Just href → do
--             resp ← fetchCard $ TL.fromStrict href
--             case resp of
--               Nothing → return Nothing
--               Just card → return $ Just [card]
--   runMaybeT $ asum $ map MaybeT [ processedOrigAuthors, hFeedAuthorCard, relAuthorCard ]
