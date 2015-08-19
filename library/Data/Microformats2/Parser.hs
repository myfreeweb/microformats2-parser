{-# LANGUAGE UnicodeSyntax, OverloadedStrings, CPP #-}

module Data.Microformats2.Parser (
  module Data.Microformats2.Parser

  -- * HTML parsing stuff (from html-conduit, xml-lens)
, documentRoot
, parseLBS
, sinkDoc
) where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Microformats2
import           Data.Microformats2.Parser.Internal
import           Data.Default
import           Data.Foldable (asum)
import           Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LB
import           Text.HTML.DOM
import           Text.XML.Lens
import           Network.URI
import           Safe (headMay)

data HtmlContentMode = Unsafe | Strip | Escape | Sanitize

(.&&.) ∷ Monad μ ⇒ μ Bool → μ Bool → μ Bool
(.&&.) = liftM2 (&&)

findMicroformat ∷ String → Element → [Element]
findMicroformat n e = e ^.. entire . hasClass n

-- | Parses all h-geo entries inside of an element.
parseGeo ∷ Element → [Geo]
parseGeo = map extractGeo . findGeo

-- | Finds all h-geo elements inside of an element.
findGeo ∷ Element → [Element]
findGeo = findMicroformat "h-geo"

-- | Parses an element as h-geo.
extractGeo ∷ Element → Geo
extractGeo e = def { geoLatitude  = extractPropertyR P "latitude"  e
                   , geoLongitude = extractPropertyR P "longitude" e
                   , geoAltitude  = extractPropertyR P "altitude"  e }


-- | Parses all h-adr entries inside of an element.
parseAdr ∷ Element → [Adr]
parseAdr = map extractAdr . findAdr

-- | Finds all h-adr elements inside of an element.
findAdr ∷ Element → [Element]
findAdr = findMicroformat "h-adr"

-- | Parses an element as h-adr.
extractAdr ∷ Element → Adr
extractAdr e = def { adrStreetAddress   = extractPropertyL P "street-address"   e
                   , adrExtendedAddress = extractPropertyL P "extended-address" e
                   , adrPostOfficeBox   = extractPropertyL P "post-office-box"  e
                   , adrLocality        = extractPropertyL P "locality"         e
                   , adrRegion          = extractPropertyL P "region"           e
                   , adrPostalCode      = extractPropertyL P "postal-code"      e
                   , adrCountryName     = extractPropertyL P "country-name"     e
                   , adrLabel           = extractPropertyL P "label"            e
                   , adrGeo             = filter (/= GeoGeo def) $
                                            (GeoGeo . extractGeo $ e) :
                                                 (TextGeo <$> extractPropertyL P "geo" e)
                                              ++ (GeoGeo . extractGeo <$> findPropertyMicroformat e "p-geo" "h-geo") }


-- | Parses all h-card entries inside of an element.
parseCard ∷ Element → [Card]
parseCard = map extractCard . findCard

-- | Finds all h-card elements inside of an element.
findCard ∷ Element → [Element]
findCard = findMicroformat "h-card"

-- | Parses an element as h-card.
extractCard ∷ Element → Card
extractCard e = def { cardName              = extractPropertyL P "name" e
                    , cardHonorificPrefix   = extractPropertyL P "honorific-prefix" e
                    , cardGivenName         = extractPropertyL P "given-name" e
                    , cardAdditionalName    = extractPropertyL P "additional-name" e
                    , cardFamilyName        = extractPropertyL P "family-name" e
                    , cardSortString        = extractPropertyL P "sort-string" e
                    , cardHonorificSuffix   = extractPropertyL P "honorific-suffix" e
                    , cardNickname          = extractPropertyL P "nickname" e
                    , cardEmail             = extractPropertyL U "email" e
                    , cardLogo              = extractPropertyL U "logo" e
                    , cardPhoto             = extractPropertyL U "photo" e
                    , cardUrl               = extractPropertyL U "url" e
                    , cardUid               = extractPropertyL U "uid" e
                    , cardCategory          = extractPropertyL P "category" e
                    , cardAdr               = filter (/= AdrAdr def) $
                                                (AdrAdr . extractAdr $ e) :
                                                     (TextAdr <$> extractPropertyL P "adr" e)
                                                  ++ ((AdrAdr . extractAdr) <$> findPropertyMicroformat e "p-adr" "h-adr")
                    , cardTel               = extractPropertyL P "tel" e
                    , cardNote              = extractPropertyL P "note" e
                    , cardBday              = extractPropertyDt "bday" e
                    , cardKey               = extractPropertyL U "key" e
                    , cardOrg               = filter (/= CardCard def) $
                                                   (TextCard <$> extractPropertyL P "org" e)
                                                ++ ((CardCard . extractCard) <$> findPropertyMicroformat e "p-org" "h-card")
                    , cardJobTitle          = extractPropertyL P "job-title" e
                    , cardRole              = extractPropertyL P "role" e
                    , cardImpp              = extractPropertyL U "impp" e
                    , cardSex               = extractPropertyL P "sex" e
                    , cardGenderIdentity    = extractPropertyL P "gender-identity" e
                    , cardAnniversary       = extractPropertyDt "anniversary" e }


-- | Parses all h-cite entries inside of an element.
parseCite ∷ HtmlContentMode → Element → [Cite]
parseCite m = map (extractCite m) . findCite

-- | Finds all h-cite elements inside of an element.
findCite ∷ Element → [Element]
findCite = findMicroformat "h-cite"

-- | Parses an element as h-cite.
extractCite ∷ HtmlContentMode → Element → Cite
extractCite m e = def { citeName        = extractPropertyL P "name" e
                      , citePublished   = extractPropertyDt "published" e
                      , citeAuthor      = extractAuthor e
                      , citeUrl         = extractPropertyL U "url" e
                      , citeUid         = extractPropertyL U "uid" e
                      , citePublication = extractPropertyL P "publication" e
                      , citeAccessed    = extractPropertyDt "accessed" e
                      , citeContent     = TextContent <$> processContent m e }


-- | Parses all h-entry entries inside of an element.
parseEntry ∷ HtmlContentMode → Element → [Entry]
parseEntry m = map (extractEntry m) . findEntry

-- | Finds all h-entry elements inside of an element.
findEntry ∷ Element → [Element]
findEntry = findMicroformat "h-entry"

-- | Parses an element as h-entry.
extractEntry ∷ HtmlContentMode → Element → Entry
extractEntry m e = def { entryName        = extractPropertyL P "name" e
                       , entrySummary     = extractPropertyL P "summary" e
                       , entryContent     = TextContent <$> processContent m e 
                       , entryPublished   = extractPropertyDt "published" e
                       , entryUpdated     = extractPropertyDt "updated" e
                       , entryAuthor      = extractAuthor e
                       , entryCategory    = extractPropertyL P "category" e
                       , entryUrl         = extractPropertyL U "url" e
                       , entryUid         = extractPropertyL U "uid" e
                       , entryLocation    = filter ((/= CardLoc def) .&&. (/= AdrLoc def) .&&. (/= GeoLoc def)) $
                                                 (CardLoc . extractCard <$> findPropertyMicroformatNotNestedIn "p-comment" e "p-location" "h-card")
                                              ++ (AdrLoc  . extractAdr  <$> findPropertyMicroformatNotNestedIn "p-comment" e "p-location" "h-adr")
                                              ++ (GeoLoc  . extractGeo  <$> findPropertyMicroformatNotNestedIn "p-comment" e "p-location" "h-geo")
                       , entryComments    = filter (/= CiteEntry def) $
                                                (CiteEntry . extractCite m <$> findPropertyMicroformat e "p-comment" "h-cite")
                                             ++ (EntryEntry . extractEntry m <$> findPropertyMicroformat e "p-comment" "h-entry")
                       , entrySyndication = extractPropertyL U "syndication" e
                       , entryInReplyTo   = UrlEntry <$> extractPropertyL U "in-reply-to" e
                       , entryLikeOf      = UrlEntry <$> extractPropertyL U "like-of" e
                       , entryRepostOf    = UrlEntry <$> extractPropertyL U "repost-of" e }

extractAuthor ∷ Element → [CardReference]
extractAuthor e = filter (/= CardCard def) $
                       (TextCard <$> extractPropertyL P "author" e)
                    ++ (CardCard . extractCard <$> findPropertyMicroformatNotNestedIn "p-comment" e "p-author" "h-card")

-- | Parses the representative h-entry on a URL, discovering its authorship <http://indiewebcamp.com/authorship>, using the HTTP fetcher function from arguments.
parseReprEntryWithAuthor ∷ Monad μ ⇒ (URI → μ (Maybe LB.ByteString)) → HtmlContentMode → TL.Text → μ (Maybe Entry)
parseReprEntryWithAuthor fetch m href = do
  case parseURI $ TL.unpack href of
    Nothing → return Nothing
    Just uri → do
      entryPage ← fetch uri
      case entryPage of
        Nothing → return Nothing
        Just body → do
          let rootEl = documentRoot $ parseLBS body
          case headMay $ parseEntry m rootEl of
            Nothing → return Nothing
            Just reprEntry → do
              discovered ← discoverAuthor fetch rootEl uri reprEntry
              return $ Just $ reprEntry { entryAuthor = fromMaybe [] discovered }

discoverAuthor ∷ Monad μ ⇒ (URI → μ (Maybe LB.ByteString)) → Element → URI → Entry → μ (Maybe [CardReference])
discoverAuthor fetch rootEl baseUri entry = do
  let fetchCard href = do
        case parseURIReference $ TL.unpack $ TL.strip href of
          Nothing → return Nothing
          Just uri → do
            resp ← fetch $ uri `relativeTo` baseUri
            case resp of
              Nothing → return Nothing
              Just body → return $ CardCard <$> (headMay $ parseCard $ documentRoot $ parseLBS body)
      fetchAuthorIfUrl (TextCard url) = fetchCard url
      fetchAuthorIfUrl c = return $ Just c
      seqListMaybe = liftM (listToMaybeList . catMaybes) . sequence
      processedOrigAuthors = seqListMaybe $ fetchAuthorIfUrl <$> entryAuthor entry
      hFeedAuthorCard = do
        -- TODO: check that the feed actually contains the entry // use stripQueryString
        case rootEl ^? entire . hasClass "h-feed" of
          Nothing → return Nothing
          Just hFeedEl → seqListMaybe $ fetchAuthorIfUrl <$> extractAuthor hFeedEl
      relAuthorCard = do
        case rootEl ^. entire . hasRel "author" . attribute "href" of
          Nothing → return Nothing
          Just href → do
            resp ← fetchCard $ TL.fromStrict href
            case resp of
              Nothing → return Nothing
              Just card → return $ Just [card]
  runMaybeT $ asum $ map MaybeT [ processedOrigAuthors, hFeedAuthorCard, relAuthorCard ]

processContent ∷ HtmlContentMode → Element → [TL.Text]
processContent Unsafe   = extractPropertyContent getAllHtml E "content"
processContent Strip    = extractPropertyContent getAllText E "content"
processContent Escape   = map (TL.replace "<" "&lt;" . TL.replace ">" "&gt;" . TL.replace "&" "&amp;") . extractPropertyContent getAllHtml E "content"
processContent Sanitize = extractPropertyContent getAllHtmlSanitized E "content"
