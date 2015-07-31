{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax, CPP #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec
import           TestCommon
import           Data.Default
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Microformats2
import           Data.Microformats2.Parser
#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec ∷ Spec
spec = do

  describe "parseGeo" $ do
    let parseGeo' = parseGeo . documentRoot . parseLBS

    it "parses valid h-geo" $ do
      parseGeo' [xml|<div>
        <p class="h-geo">
          <span class="p-latitude">37.33168</span>
          <span class="p-longitude">-122.03016</span>
          <span class="p-altitude">1.2345</span>
        </p>
        <p class="h-geo">
          <data class="p-latitude" value="123.45">
          <input class="p-latitude" value="678.9">
        </p>
      </div>|] `shouldBe` [ def { geoLatitude = pure 37.33168, geoLongitude = pure (-122.03016), geoAltitude = pure 1.2345 }
                          , def { geoLatitude = [123.45, 678.9] } ]

    it "ignores invalid properties" $ do
      parseGeo' [xml|<p class="h-geo">
          <span class="p-latitude">HELLO WORLD!!</span>
          <span class="p-altitude">1.2345</span>
        </p>|] `shouldBe` [ def { geoAltitude = pure 1.2345 } ]

  describe "parseAdr" $ do
    let parseAdr' = parseAdr . documentRoot . parseLBS

    it "parses valid h-adr" $ do
      parseAdr' [xml|<div>
          <article class="h-adr">
            <span class="p-street-address">SA</span>
            <p class="p-extended-address">EA</p>
            <abbr class="p-post-office-box" title="PO">_</abbr>
            <span class="p-locality">L</span>
            <span class="p-region">R</span>
            <span class="p-postal-code">PC</span>
            <span class="p-country-name">C</span>
            <span class="p-label">LB</span>
            <span class="p-geo">G</span>
          </article>
        </div>|] `shouldBe` [ def { adrStreetAddress = pure "SA", adrExtendedAddress = pure "EA"
                                  , adrPostOfficeBox = pure "PO", adrLocality = pure "L"
                                  , adrRegion = pure "R", adrPostalCode = pure "PC"
                                  , adrCountryName = pure "C", adrLabel = pure "LB"
                                  , adrGeo = pure $ TextGeo "G" } ]

    it "parses p-geo" $ do
      parseAdr' [xml|<div>
          <span class="h-adr">
            <span class="p-geo h-geo">
              <span class="p-latitude">37.33168</span>
              <span class="p-longitude">-122.03016</span>
              <span class="p-altitude">1.2345</span>
            </span>
          </span>
        </div>|] `shouldBe` [ def { adrGeo = [GeoGeo $ def { geoLatitude = pure 37.33168, geoLongitude = pure (-122.03016), geoAltitude = pure 1.2345  }] } ]

    it "ignores nested h-geo not marked as p-geo" $ do
      parseAdr' [xml|<div>
          <span class="h-adr">
            <span class="h-geo"><span class="p-altitude">1.2345</span></span>
          </span>
        </div>|] `shouldBe` [ def ]

    it "parses geo properties into p-geo" $ do
      parseAdr' [xml|<div>
          <span class="h-adr">
            <span class="p-latitude">37.33168</span>
            <span class="p-longitude">-122.03016</span>
            <span class="p-altitude">1.2345</span>
          </span>
        </div>|] `shouldBe` [ def { adrGeo = [GeoGeo $ def { geoLatitude = pure 37.33168, geoLongitude = pure (-122.03016), geoAltitude = pure 1.2345  }] } ]

  describe "parseCard" $ do
    let parseCard' = parseCard . documentRoot . parseLBS

    it "parses valid h-card" $ do
      parseCard' [xml|<div>
          <p class="h-card">
            <img class="u-photo" src="photo.png" alt="" />
            <a class="p-name u-url" href="http://example.org">Joe Bloggs</a>
            <a class="u-email" href="mailto:joebloggs@example.com">joebloggs@example.com</a>,
          </p>
        </div>|] `shouldBe` [ def { cardPhoto = pure "photo.png"
                                  , cardUrl = pure "http://example.org"
                                  , cardName = pure "Joe Bloggs"
                                  , cardEmail = pure "mailto:joebloggs@example.com" } ]

    it "parses valid implied h-card" $ do
      parseCard' [xml|<div>
          <a class="h-card" href="http://example.org">Joe Bloggs</a>
          <img class="h-card" src="http://example.org/photo.jpg" />
        </div>|] `shouldBe` [ def { cardUrl = pure "http://example.org"
                                  , cardName = pure "Joe Bloggs" }
                            , def { cardPhoto = pure "http://example.org/photo.jpg" }]

    it "parses p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-adr h-adr">
              <span class="p-street-address">17</span>
              <span class="p-locality">Reykjavik</span>
              <span class="p-country-name">Iceland</span>
            </p>
          </section>
        </div>|] `shouldBe` [ def { cardAdr = pure (AdrAdr $ def { adrStreetAddress = pure "17"
                                                                 , adrLocality = pure "Reykjavik"
                                                                 , adrCountryName = pure "Iceland" }) } ]

    it "ignores nested h-adr not marked as p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="h-adr"> <span class="p-country-name">Iceland</span> </p>
          </section>
        </div>|] `shouldBe` [ def ]

    it "parses adr and geo properties into p-adr" $ do
      parseCard' [xml|<div>
          <p class="h-card">
            <span class="p-street-address">17</span>
            <span class="p-locality">Reykjavik</span>
            <span class="p-country-name">Iceland</span>
            <span class="p-longitude">-122.03016</span>
          </p>
        </div>|] `shouldBe` [ def { cardAdr = pure (AdrAdr $ def { adrStreetAddress = pure "17"
                                                                 , adrLocality = pure "Reykjavik"
                                                                 , adrCountryName = pure "Iceland"
                                                                 , adrGeo = [ GeoGeo $ def { geoLongitude = pure (-122.03016) } ] }) } ]

    it "parses h-geo into p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-geo h-geo"> <span class="p-longitude">-122.03016</span> </p>
          </section>
        </div>|] `shouldBe` [ def { cardAdr = pure (AdrAdr $ def { adrGeo = [ GeoGeo $ def { geoLongitude = pure (-122.03016) } ] }) } ]

    it "ignores nested h-geo not marked as p-geo" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="h-geo"> <span class="p-longitude">-122.03016</span> </p>
          </section>
        </div>|] `shouldBe` [ def ]

    it "parses multiple things into p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-geo h-geo"> <span class="p-longitude">-122.03016</span> </p>
            <span class="p-altitude">-122.03016</span>
            <span class="p-country-name">Iceland</span>
            <p class="p-adr h-adr"> <span class="p-locality">Reykjavik</span> </p>
          </section>
        </div>|] `shouldBe` [ def { cardAdr = [
                                      (AdrAdr $ def { adrCountryName = pure "Iceland" 
                                                    , adrGeo = [ GeoGeo $ def { geoAltitude = pure (-122.03016) }
                                                               , GeoGeo $ def { geoLongitude = pure (-122.03016) } ] })
                                    , (AdrAdr $ def { adrLocality = pure "Reykjavik" }) ]} ]


    it "parses p-org" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-org h-card"> <span class="p-name">IndieWebCamp</span> </p>
            <span class="p-org">Microformats</span>
          </section>
        </div>|] `shouldBe` [ def { cardOrg = [ TextCard "Microformats"
                                                , (CardCard $ def { cardName = pure "IndieWebCamp" }) ] }
                            , def { cardName = pure "IndieWebCamp" }]

  describe "parseCite" $ do
    let parseCite' = parseCite Strip . documentRoot . parseLBS

    it "parses valid h-cite" $ do
      parseCite' [xml|<div>
          <article class="h-cite">
            <a class="p-name u-url u-uid" href="https://youtu.be/E99FnoYqoII">Rails is Omakase</a>
            <span class="p-author h-card"><span class="p-name">DHH</span></span>
            <time class="dt-published">2013-01-25</time>
            <p class="e-content">Rails is not that. Rails is omakase...</p>
          </article>
        </div>|] `shouldBe` [ def { citeName = pure "Rails is Omakase"
                                  , citeUrl = pure "https://youtu.be/E99FnoYqoII"
                                  , citeUid = pure "https://youtu.be/E99FnoYqoII"
                                  , citeAuthor = pure $ CardCard $ def { cardName = pure "DHH" }
                                  , citeContent = pure $ TextContent "Rails is not that. Rails is omakase..."
                                  , citePublished = pure $ UTCTime (fromGregorian 2013 1 25) (secondsToDiffTime 0) } ]


  describe "parseEntry" $ do
    let parseEntry' m = parseEntry m . documentRoot . parseLBS

    it "parses valid h-entry" $ do
      parseEntry' Strip [xml|<div>
          <article class="h-entry">
            <a class="p-name u-url u-uid" href="https://youtu.be/E99FnoYqoII">Rails is Omakase</a>
            <span class="p-author h-card"><span class="p-name">DHH</span></span>
            <a href="http://david.heinemeierhansson.com" class="p-author h-card">DHH</a>
            <a href="http://david.heinemeierhansson.com" class="p-author">David</a>
            <time class="dt-published">2013-01-25</time>
            <time class="dt-updated">2013-01-25T01:23</time>
            <p class="e-content">Rails is not that. Rails is omakase...</p>
          </article>
        </div>|] `shouldBe` [ def { entryName = pure "Rails is Omakase"
                                  , entryUrl = pure "https://youtu.be/E99FnoYqoII"
                                  , entryUid = pure "https://youtu.be/E99FnoYqoII"
                                  , entryAuthor = [ TextCard "David"
                                                  , CardCard $ def { cardName = pure "DHH" }
                                                  , CardCard $ def { cardName = pure "DHH", cardUrl = pure "http://david.heinemeierhansson.com" } ]
                                  , entryContent = pure $ TextContent "Rails is not that. Rails is omakase..."
                                  , entryPublished = pure $ UTCTime (fromGregorian 2013 1 25) (secondsToDiffTime 0)
                                  , entryUpdated = pure $ UTCTime (fromGregorian 2013 1 25) (secondsToDiffTime 4980) } ]

    it "supports different html content modes" $ do
      let src = [xml|<div>
          <article class="h-entry">
            <p class="e-content"><script>alert('XSS')</script><a href="http://rubyonrails.org" onclick="alert()">Rails</a> is not that. Rails is omakase...</p>
          </article>
        </div>|]
      parseEntry' Unsafe   src `shouldBe` [ def { entryContent = pure $ TextContent "<script>alert('XSS')</script><a href=\"http://rubyonrails.org\" onclick=\"alert()\">Rails</a> is not that. Rails is omakase..." } ]
      parseEntry' Strip    src `shouldBe` [ def { entryContent = pure $ TextContent "alert('XSS')Rails is not that. Rails is omakase..." } ]
      parseEntry' Escape   src `shouldBe` [ def { entryContent = pure $ TextContent "&lt;script&gt;alert('XSS')&lt;/script&gt;&lt;a href=\"http://rubyonrails.org\" onclick=\"alert()\"&gt;Rails&lt;/a&gt; is not that. Rails is omakase..." } ]
      parseEntry' Sanitize src `shouldBe` [ def { entryContent = pure $ TextContent "<a href=\"http://rubyonrails.org\">Rails</a> is not that. Rails is omakase..." } ]

    it "parses p-location" $ do
      parseEntry' Strip [xml|<div>
          <article class="h-entry">
            <p class="p-location h-card">
              <a class="p-name u-url" href="http://penisland.net">Pen Island</a>
            </p>
            <p class="p-location h-adr">
              <span class="p-country-name">USA</span>
            </p>
            <p class="p-location h-geo">
              <data class="p-latitude" value="123.45">
            </p>
          </article>
        </div>|] `shouldBe` [ def { entryLocation = [ CardLoc $ def { cardName = pure "Pen Island", cardUrl = pure "http://penisland.net" }
                                                    , AdrLoc $ def { adrCountryName = pure "USA" }
                                                    , GeoLoc $ def { geoLatitude = pure 123.45 } ] } ]

    it "parses p-comment" $ do
      (take 1 $ parseEntry' Strip [xml|<div>
          <article class="h-entry">
            <div class="p-comment h-cite">
              <a class="p-name">Rails is Omakase</a>
              <!-- p-author h-card must not propagate to the h-entry's p-author! -->
              <a href="http://david.heinemeierhansson.com" class="p-author h-card">David</a>
              <time class="dt-published">2013-01-25</time>
              <p class="e-content">Rails is not that. Rails is omakase...</p>
            </div>
            <div class="p-comment h-entry">
              <a class="p-name">Rails is Omakase</a>
              <time class="dt-published">2013-01-25</time>
              <p class="e-content">Rails is not that. Rails is omakase...</p>
            </div>
          </article>
        </div>|]) `shouldBe` [ def { entryComments = [ CiteEntry $ def { citeName = pure "Rails is Omakase"
                                                                       , citeAuthor = [ CardCard $ def { cardName = pure "David", cardUrl = pure "http://david.heinemeierhansson.com" } ]
                                                                       , citeContent = pure $ TextContent "Rails is not that. Rails is omakase..."
                                                                       , citePublished = pure $ UTCTime (fromGregorian 2013 1 25) (secondsToDiffTime 0) }
                                                     , EntryEntry $ def { entryName = pure "Rails is Omakase"
                                                                        , entryContent = pure $ TextContent "Rails is not that. Rails is omakase..."
                                                                        , entryPublished = pure $ UTCTime (fromGregorian 2013 1 25) (secondsToDiffTime 0) }                                                                      ] } ]
