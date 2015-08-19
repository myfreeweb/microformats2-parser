{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE CPP, RankNTypes #-}

module Data.Microformats2.Parser.Property where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Foldable (asum)
import qualified Data.Map as M
import           Data.Maybe
-- import           Data.Time.Format
import           Text.XML.Lens hiding (re)
import           Data.Microformats2.Parser.HtmlUtil
import           Data.Microformats2.Parser.Util

unwrapName ∷ (Name, a) → (Text, a)
unwrapName (Name n _ _, val) = (n, val)

notProperty ∷ Element → Bool
notProperty (Element _ a _) = not $ any isProperty $ T.splitOn " " $ fromMaybe "" $ lookup "class" $ map unwrapName $ M.toList a
  where isProperty x = T.isPrefixOf "p-" x || T.isPrefixOf "u-" x || T.isPrefixOf "e-" x || T.isPrefixOf "dt-" x || T.isPrefixOf "h-" x
notProperty _ = True

hasOneClass ∷ [String] → Traversal' Element Element
hasOneClass ns = attributeSatisfies "class" $ \a → any (\x → (T.pack x) `elem` (T.splitOn " " a)) ns

hasClass ∷ String → Traversal' Element Element
hasClass n = attributeSatisfies "class" $ \a → (T.pack n) `elem` (T.splitOn " " a)

getOnlyChildren ∷ Element → [Element]
getOnlyChildren e = if lengthOf plate e == 1 then e ^.. plate else []

getOnlyChild ∷ Name → Element → Maybe Element
getOnlyChild n e = if' (fromMaybe False $ notProperty <$> r) $ r
  where r = if' (lengthOf plate e == 1) $ e ^? plate . el n

getOnlyOfType ∷ Name → Element → Maybe Element
getOnlyOfType n e = if' (fromMaybe False $ notProperty <$> r) $ r
  where r = if' (lengthOf (plate . el n) e == 1) $ e ^? plate . el n

els ∷ [Name] → Traversal' Element Element
els ns f s = if elementName s `elem` ns then f s else pure s

getAbbrTitle ∷ Element → Maybe Text
getAbbrTitle e = e ^. el "abbr" . attribute "title"

getDataInputValue ∷ Element → Maybe Text
getDataInputValue e = e ^. els ["data", "input"] . attribute "value"

getImgSrc ∷ Element → Maybe Text
getImgSrc e = e ^. el "img" . attribute "src"

getObjectData ∷ Element → Maybe Text
getObjectData e = e ^. el "object" . attribute "data"

getImgAreaAlt ∷ Element → Maybe Text
getImgAreaAlt e = e ^. els ["img", "area"] . attribute "alt"

getAAreaHref ∷ Element → Maybe Text
getAAreaHref e = e ^. els ["a", "area"] . attribute "href"

getImgAudioVideoSourceSrc ∷ Element → Maybe Text
getImgAudioVideoSourceSrc e = e ^. els ["img", "audio", "video", "source"] . attribute "src"

getTimeInsDelDatetime ∷ Element → Maybe Text
getTimeInsDelDatetime e = e ^. els ["time", "ins", "del"] . attribute "datetime"

getOnlyChildImgAreaAlt ∷ Element → Maybe Text
getOnlyChildImgAreaAlt e = (^. attribute "alt") =<< asum (getOnlyChild <$> [ "img", "area" ] <*> pure e)

getOnlyChildAbbrTitle ∷ Element → Maybe Text
getOnlyChildAbbrTitle e = (^. attribute "title") =<< getOnlyChild "abbr" e

getOnlyOfTypeImgSrc ∷ Element → Maybe Text
getOnlyOfTypeImgSrc e = (^. attribute "src") =<< getOnlyOfType "img" e

getOnlyOfTypeObjectData ∷ Element → Maybe Text
getOnlyOfTypeObjectData e = (^. attribute "data") =<< getOnlyOfType "object" e

getOnlyOfTypeAAreaHref ∷ Element → Maybe Text
getOnlyOfTypeAAreaHref e = (^. attribute "href") =<< asum (getOnlyOfType <$> [ "a", "area" ] <*> pure e)

extractValue ∷ Element → Maybe Text
extractValue e = asum $ [ getAbbrTitle, getDataInputValue, getImgAreaAlt, getInnerTextRaw ] <*> pure e

extractValueTitle ∷ Element → Maybe Text
extractValueTitle e = if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"

extractValueClassPattern ∷ [Element → Maybe Text] → Element → Maybe Text
extractValueClassPattern fs e = if' (isJust $ e ^? valueParts) extractValueParts
  where extractValueParts   = Just . T.concat . catMaybes $ e ^.. valueParts . to extractValuePart
        extractValuePart e' = asum $ fs <*> pure e'
        valueParts          ∷ Applicative f => (Element → f Element) → Element → f Element
        valueParts          = entire . hasOneClass ["value", "value-title"]

data PropType = P | U | Dt

extractProperty ∷ PropType → Element → Maybe Text
extractProperty P e =
  asum $ [ extractValueClassPattern [extractValueTitle, extractValue]
         , getAbbrTitle, getDataInputValue, getImgAreaAlt, getInnerTextWithImgs ] <*> pure e
extractProperty U e =
  asum $ [ getAAreaHref, getImgAudioVideoSourceSrc
         , extractValueClassPattern [extractValueTitle, extractValue]
         , getAbbrTitle, getDataInputValue, getInnerTextRaw ] <*> pure e
extractProperty Dt e =
  asum $ (extractValueClassPattern ms : ms ++ [getInnerTextRaw]) <*> pure e
  where ms = [ getTimeInsDelDatetime, getAbbrTitle, getDataInputValue ]

-- parsePropertyDateTime ∷ ParseTime α ⇒ Element → Maybe α
-- parsePropertyDateTime e = readISO <$> T.unpack <$> extractProperty Dt e
--   where readISO x = asum $ map ($ x) [ isoParse $ Just "%H:%M:%S%z"
--                                      , isoParse $ Just "%H:%M:%SZ"
--                                      , isoParse $ Just "%H:%M:%S"
--                                      , isoParse $ Just "%H:%M"
--                                      , parseTimeD "%G-W%V-%u"
--                                      , parseTimeD "%G-W%V"
--                                      , isoParse Nothing ]
--         isoParse = parseTimeD . iso8601DateFormat
--         parseTimeD = parseTimeM True defaultTimeLocale

implyProperty ∷ PropType → String → Element → Maybe Text
implyProperty P "name"  e = asum $ [ getImgAreaAlt, getAbbrTitle
                                   , getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle
                                   , \e' -> asum $ [ getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle ] <*> getOnlyChildren e'
                                   , getInnerTextRaw ] <*> pure e
implyProperty U "photo" e = asum $ [ getImgSrc, getObjectData
                                   , getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData
                                   , \e' -> asum $ [ getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData ] <*> getOnlyChildren e'
                                   ] <*> pure e
implyProperty U "url"   e = asum $ [ getAAreaHref, getOnlyOfTypeAAreaHref ] <*> pure e
implyProperty _ _ _ = Nothing
