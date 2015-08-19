{-# LANGUAGE UnicodeSyntax, OverloadedStrings, CPP #-}

module Data.Microformats2.Parser (
  -- module Data.Microformats2.Parser

  -- * HTML parsing stuff (from html-conduit, xml-lens)
  documentRoot
, parseLBS
, sinkDoc
) where

import           Text.HTML.DOM
import           Text.XML.Lens
#if __GLASGOW_HASKELL__ < 709
-- import           Control.Applicative
#endif
-- import           Control.Monad
-- import           Control.Monad.Trans.Maybe
-- import           Data.Microformats2
-- import           Data.Microformats2.Parser.Property
-- import           Data.Microformats2.Parser.HtmlUtil
-- import           Data.Microformats2.Parser.Util
-- import           Data.Default
-- import           Data.Foldable (asum)
-- import           Data.Maybe
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.ByteString.Lazy as LB
-- import           Network.URI
-- import           Safe (headMay)

-- -- | Parses the representative h-entry on a URL, discovering its authorship <http://indiewebcamp.com/authorship>, using the HTTP fetcher function from arguments.
-- parseReprEntryWithAuthor ∷ Monad μ ⇒ (URI → μ (Maybe LB.ByteString)) → HtmlContentMode → TL.Text → μ (Maybe Entry)
-- parseReprEntryWithAuthor fetch m href = do
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
