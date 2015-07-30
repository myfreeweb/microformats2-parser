{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Data.Microformats2.Parser (
  module Data.Microformats2.Parser

  -- * HTML parsing stuff (from html-conduit, xml-lens)
, documentRoot
, parseLBS
, sinkDoc
) where

import           Control.Applicative
import           Control.Monad
import           Data.Microformats2
import           Data.Microformats2.Parser.Internal
import           Data.Default
-- import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Text.HTML.DOM
import           Text.XML.Lens

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
                                                  ++ (map (AdrAdr . extractAdr) $ findPropertyMicroformat e "p-adr" "h-adr")
                    , cardTel               = extractPropertyL P "tel" e
                    , cardNote              = extractPropertyL P "note" e
                    , cardBday              = extractPropertyDt "bday" e
                    , cardKey               = extractPropertyL U "key" e
                    , cardOrg               = filter (/= CardCard def) $
                                                   (TextCard <$> extractPropertyL P "org" e)
                                                ++ (map (CardCard . extractCard) $ findPropertyMicroformat e "p-org" "h-card")
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
                      , citeAuthor      = filter (/= CardCard def) $
                                               (TextCard <$> extractPropertyL P "author" e)
                                            ++ (CardCard . extractCard <$> findPropertyMicroformat e "p-author" "h-card")
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
                       , entryAuthor      = filter (/= CardCard def) $
                                                (TextCard <$> extractPropertyL P "author" e)
                                             ++ (CardCard . extractCard <$> findPropertyMicroformat e "p-author" "h-card")
                       , entryCategory    = extractPropertyL P "category" e
                       , entryUrl         = extractPropertyL U "url" e
                       , entryUid         = extractPropertyL U "uid" e
                       , entryLocation    = filter ((/= CardLoc def) .&&. (/= AdrLoc def) .&&. (/= GeoLoc def)) $
                                                 (CardLoc . extractCard <$> findPropertyMicroformat e "p-location" "h-card")
                                              ++ (AdrLoc  . extractAdr <$> findPropertyMicroformat e "p-location" "h-adr")
                                              ++ (GeoLoc  . extractGeo <$> findPropertyMicroformat e "p-location" "h-geo")
                       , entryComments    = filter (/= CiteEntry def) $
                                             (CiteEntry . extractCite m <$> findPropertyMicroformat e "p-comment" "h-cite")
                       , entrySyndication = extractPropertyL U "syndication" e
                       , entryInReplyTo   = UrlEntry <$> extractPropertyL U "in-reply-to" e
                       , entryLikeOf      = UrlEntry <$> extractPropertyL U "like-of" e
                       , entryRepostOf    = UrlEntry <$> extractPropertyL U "repost-of" e }

processContent ∷ HtmlContentMode → Element → [LT.Text]
processContent Unsafe   = extractPropertyContent getAllHtml P "content"
processContent Strip    = extractPropertyContent getAllText P "content"
processContent Escape   = map (LT.replace "<" "&lt;" . LT.replace ">" "&gt;" . LT.replace "&" "&amp;") . extractPropertyContent getAllHtml P "content"
processContent Sanitize = extractPropertyContent getAllHtmlSanitized P "content"
