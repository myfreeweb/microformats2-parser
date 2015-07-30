{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}

module Data.Microformats2.Parser.Internal where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Foldable (asum)
import           Data.Maybe
import           Control.Applicative
import           Text.XML.Lens hiding (re)
import           Text.Blaze
import           Text.Blaze.Renderer.Text
import           Text.Regex.PCRE.Heavy
import           Safe (readMay)

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

notMicroformat ∷ Traversal' Element Element
notMicroformat = attributeSatisfies "class" $ not . (≈ [re|h-\w+|])

entireNotMicroformat ∷ Traversal' Element Element
entireNotMicroformat f e@(Element _ _ ns) = com <$> f e <*> traverse (_Element (notMicroformat $ entireNotMicroformat f)) ns
  where com (Element n a _) = Element n a

hasOneClass ∷ [String] → Traversal' Element Element
hasOneClass ns = attributeSatisfies "class" $ \a → any (\x → T.isInfixOf (T.pack x) a) ns

hasClass ∷ String →  Traversal' Element Element
hasClass n = attributeSatisfies "class" $ T.isInfixOf . T.pack $ n

getOnlyChildren ∷ Element → [Element]
getOnlyChildren e = if ((lengthOf plate e) == 1) then e ^.. plate else []

getOnlyChild ∷ Name → Element → Maybe Element
getOnlyChild n e = if' ((lengthOf plate e) == 1) $ e ^? plate . el n

getOnlyOfType ∷ Name → Element → Maybe Element
getOnlyOfType n e = if' ((lengthOf (plate . el n) e) == 1) $ e ^? plate . el n

els ∷ [Name] → Traversal' Element Element
els ns f s = if (elementName s) `elem` ns then f s else pure s

removeWhitespace ∷ Text → Text
removeWhitespace = gsub [re|(\s+|&nbsp;)|] (" " ∷ String) -- lol vim |||||||

_Content' ∷ Prism' Node Text
_Content' = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  _ → Nothing

_InnerHtml ∷ Prism' Node Text
_InnerHtml = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → Just . TL.toStrict . renderMarkup . toMarkup $ e
  _ → Nothing

_ContentWithAlts ∷ Prism' Node Text
_ContentWithAlts = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → e ^. el "img" . attribute "alt"
  _ → Nothing

basicText ∷ Traversal' Element Text
basicText = entire . nodes . traverse . _Content'

getText ∷ Element → Maybe Text
getText e = Just . T.strip <$> T.concat $ e ^.. basicText

getAllText ∷ Element → Maybe Text
getAllText e = Just . T.strip <$> T.concat $ e ^.. entire . nodes . traverse . _ContentWithAlts

getAllHtml ∷ Element → [Text]
getAllHtml e = [T.strip <$> T.concat $ e ^.. nodes . traverse . _InnerHtml]

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
getOnlyChildImgAreaAlt e = (^. attribute "alt") =<< (asum $ getOnlyChild <$> [ "img", "area" ] <*> pure e)

getOnlyChildAbbrTitle ∷ Element → Maybe Text
getOnlyChildAbbrTitle e = (^. attribute "title") =<< getOnlyChild "abbr" e

getOnlyOfTypeImgSrc ∷ Element → Maybe Text
getOnlyOfTypeImgSrc e = (^. attribute "src") =<< getOnlyOfType "img" e

getOnlyOfTypeObjectData ∷ Element → Maybe Text
getOnlyOfTypeObjectData e = (^. attribute "data") =<< getOnlyOfType "object" e

getOnlyOfTypeAAreaHref ∷ Element → Maybe Text
getOnlyOfTypeAAreaHref e = (^. attribute "href") =<< (asum $ getOnlyOfType <$> [ "a", "area" ] <*> pure e)

extractValue ∷ Element → Maybe Text
extractValue e = asum $ [ getAbbrTitle, getDataInputValue, getImgAreaAlt, getAllText ] <*> pure e

extractValueTitle ∷ Element → Maybe Text
extractValueTitle e = if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"

extractValueClassPattern ∷ [Element → Maybe Text] → Element → Maybe Text
extractValueClassPattern fs e = if' (isJust $ e ^? valueParts) $ extractValueParts
  where extractValueParts   = Just . T.concat . catMaybes $ e ^.. valueParts . to extractValuePart
        extractValuePart e' = asum $ fs <*> pure e'
        valueParts          ∷ Applicative f => (Element → f Element) → Element → f Element
        valueParts          = entire . hasOneClass ["value", "value-title"]

findProperty ∷ Element → String → [Element]
findProperty e n = filter (/= e) $ e ^.. entireNotMicroformat . hasClass n

findPropertyMicroformat ∷ Element → String → [Element]
findPropertyMicroformat e n = filter (/= e) $ e ^.. entire . hasClass n

data PropType = P | U | Dt | E

extract ∷ [Element → Maybe Text] → Element → [Text]
extract ps e = catMaybes $ [ \x -> asum $ ps <*> pure x ] <*> pure e

extractProperty ∷ PropType → String → Element → [Text]
extractProperty P n e' =
  findProperty e' ("p-" ++ n) >>=
  extract [ extractValueClassPattern [extractValueTitle, extractValue]
          , extractValue ]
extractProperty U n e' =
  findProperty e' ("u-" ++ n) >>=
  extract [ getAAreaHref, getImgAudioVideoSourceSrc
          , extractValueClassPattern [extractValueTitle, extractValue]
          , getAbbrTitle, getDataInputValue, getAllText ]
extractProperty Dt n e' =
  findProperty e' ("dt-" ++ n) >>=
  extract ((extractValueClassPattern ms) : ms ++ [getAllText])
  where ms = [ getTimeInsDelDatetime, getAbbrTitle, getDataInputValue ]
extractProperty E n e' = findProperty e' ("e-" ++ n) >>= getAllHtml

extractPropertyL ∷ PropType → String → Element → [TL.Text]
extractPropertyL t n e = TL.fromStrict <$> extractProperty t n e

extractPropertyR ∷ Read α ⇒ PropType → String → Element → [α]
extractPropertyR t n e = catMaybes $ readMay <$> T.unpack <$> extractProperty t n e

implyProperty ∷ PropType → String → Element → Maybe Text
implyProperty P "name"  e = asum $ [ getImgAreaAlt, getAbbrTitle
                                   , getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle
                                   , \e' -> asum $ [ getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle ] <*> getOnlyChildren e'
                                   , getText ] <*> pure e
implyProperty U "photo" e = asum $ [ getImgSrc, getObjectData
                                   , getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData
                                   , \e' -> asum $ [ getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData ] <*> getOnlyChildren e'
                                   ] <*> pure e
implyProperty U "url"   e = asum $ [ getAAreaHref, getOnlyOfTypeAAreaHref ] <*> pure e
implyProperty _ _ _ = Nothing
