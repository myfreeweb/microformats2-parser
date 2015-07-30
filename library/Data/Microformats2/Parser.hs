{-# LANGUAGE UnicodeSyntax #-}

module Data.Microformats2.Parser (
  module Data.Microformats2.Parser

  -- * HTML parsing stuff (from html-conduit, xml-lens)
, documentRoot
, parseLBS
, sinkDoc
) where

-- import qualified Data.Text as T
import           Data.Microformats2
import           Data.Microformats2.Parser.Internal
import           Data.Default
import           Control.Applicative
import           Text.HTML.DOM
import           Text.XML.Lens

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
                                              ++ (map (GeoGeo . extractGeo) $ findPropertyMicroformat e "p-geo") }


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
                                                  ++ (map (AdrAdr . extractAdr) $ findPropertyMicroformat e "p-adr")
                    , cardTel               = extractPropertyL P "tel" e
                    , cardNote              = extractPropertyL P "note" e
                    , cardBday              = extractPropertyDt "bday" e
                    , cardKey               = extractPropertyL U "key" e
                    , cardOrg               = filter (/= CardCard def) $
                                                (TextCard <$> extractPropertyL P "org" e)
                                                ++ (map (CardCard . extractCard) $ findPropertyMicroformat e "p-org")
                    , cardJobTitle          = extractPropertyL P "job-title" e
                    , cardRole              = extractPropertyL P "role" e
                    , cardImpp              = extractPropertyL U "impp" e
                    , cardSex               = extractPropertyL P "sex" e
                    , cardGenderIdentity    = extractPropertyL P "gender-identity" e
                    , cardAnniversary       = extractPropertyDt "anniversary" e }


-- | Parses all h-cite entries inside of an element.
parseCite ∷ Element → [Cite]
parseCite = map extractCite . findCite

-- | Finds all h-cite elements inside of an element.
findCite ∷ Element → [Element]
findCite = findMicroformat "h-cite"

-- | Parses an element as h-cite.
extractCite ∷ Element → Cite
extractCite e = def { citeName        = extractPropertyL P "name" e
                    , citePublished   = extractPropertyDt "published" e
                    , citeAuthor      = filter (/= CardCard def) $
                                          (TextCard <$> extractPropertyL P "author" e)
                                          ++ (map (CardCard . extractCard) $ findPropertyMicroformat e "p-author")
                    , citeUrl         = extractPropertyL U "url" e
                    , citeUid         = extractPropertyL U "uid" e
                    , citePublication = extractPropertyL P "publication" e
                    , citeAccessed    = extractPropertyDt "accessed" e
                    , citeContent     = map TextContent $ extractPropertyL P "content" e }
