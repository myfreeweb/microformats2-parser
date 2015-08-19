{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE CPP, RankNTypes #-}

module Data.Microformats2.Parser.Property where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isSpace)
import           Data.Foldable (asum)
import qualified Data.Map as M
import           Data.Maybe
import           Text.XML.Lens hiding (re)
import           Data.Microformats2.Parser.HtmlUtil
import           Data.Microformats2.Parser.Util

unwrapName ∷ (Name, a) → (Text, a)
unwrapName (Name n _ _, val) = (n, val)

classes ∷ Element → [Text]
classes (Element _ as _) = T.split isSpace . fromMaybe "" . lookup "class" . map unwrapName . M.toList $ as

isPClass, isUClass, isEClass, isDtClass, isPropertyClass, isMf2Class ∷ Text → Bool
isPClass          = T.isPrefixOf "p-"
isUClass          = T.isPrefixOf "u-"
isEClass          = T.isPrefixOf "e-"
isDtClass         = T.isPrefixOf "dt-"
isPropertyClass x = isPClass x || isUClass x || isEClass x || isDtClass x
isMf2Class        = T.isPrefixOf "h-"

isProperty ∷ Element → Bool
isProperty = any isPropertyClass . classes

propertyElements ∷ Traversal' Element Element
propertyElements = attributeSatisfies "class" $ any isPropertyClass . T.split isSpace

hasOneClass ∷ [String] → Traversal' Element Element
hasOneClass ns = attributeSatisfies "class" $ \a → any (\x → (T.pack x) `elem` (T.split isSpace a)) ns

hasClass ∷ String → Traversal' Element Element
hasClass n = attributeSatisfies "class" $ \a → (T.pack n) `elem` (T.split isSpace a)

getOnlyChildren ∷ Element → [Element]
getOnlyChildren e = if lengthOf plate e == 1 then e ^.. plate else []

getOnlyChild, getOnlyOfType ∷ Name → Element → Maybe Element
getOnlyChild n e = if' (fromMaybe False $ not <$> isProperty <$> r) $ r
  where r = if' (lengthOf plate e == 1) $ e ^? plate . el n
getOnlyOfType n e = if' (fromMaybe False $ not <$> isProperty <$> r) $ r
  where r = if' (lengthOf (plate . el n) e == 1) $ e ^? plate . el n

els ∷ [Name] → Traversal' Element Element
els ns f s = if elementName s `elem` ns then f s else pure s

getAbbrTitle, getDataInputValue, getImgSrc, getObjectData, getImgAreaAlt, getAAreaHref, getImgAudioVideoSourceSrc, getTimeInsDelDatetime, getOnlyChildImgAreaAlt, \
getOnlyChildAbbrTitle, getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData, getOnlyOfTypeAAreaHref, extractValue, extractValueTitle ∷ Element → Maybe Text
getAbbrTitle              e = e ^. el "abbr" . attribute "title"
getDataInputValue         e = e ^. els ["data", "input"] . attribute "value"
getImgSrc                 e = e ^. el "img" . attribute "src"
getObjectData             e = e ^. el "object" . attribute "data"
getImgAreaAlt             e = e ^. els ["img", "area"] . attribute "alt"
getAAreaHref              e = e ^. els ["a", "area"] . attribute "href"
getImgAudioVideoSourceSrc e = e ^. els ["img", "audio", "video", "source"] . attribute "src"
getTimeInsDelDatetime     e = e ^. els ["time", "ins", "del"] . attribute "datetime"
getOnlyChildImgAreaAlt    e = (^. attribute "alt") =<< asum (getOnlyChild <$> [ "img", "area" ] <*> pure e)
getOnlyChildAbbrTitle     e = (^. attribute "title") =<< getOnlyChild "abbr" e
getOnlyOfTypeImgSrc       e = (^. attribute "src") =<< getOnlyOfType "img" e
getOnlyOfTypeObjectData   e = (^. attribute "data") =<< getOnlyOfType "object" e
getOnlyOfTypeAAreaHref    e = (^. attribute "href") =<< asum (getOnlyOfType <$> [ "a", "area" ] <*> pure e)
extractValue              e = asum $ [ getAbbrTitle, getDataInputValue, getImgAreaAlt, getInnerTextRaw ] <*> pure e
extractValueTitle         e = if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"

extractValueClassPattern ∷ [Element → Maybe Text] → Element → Maybe Text
extractValueClassPattern fs e = if' (isJust $ e ^? valueParts) extractValueParts
  where extractValueParts   = Just . T.concat . catMaybes $ e ^.. valueParts . to extractValuePart
        extractValuePart e' = asum $ fs <*> pure e'
        valueParts          ∷ Applicative f => (Element → f Element) → Element → f Element
        valueParts          = entire . hasOneClass ["value", "value-title"]

extractP, extractU, extractDt ∷ Element → Maybe Text
extractP e =
  asum $ [ extractValueClassPattern [extractValueTitle, extractValue]
         , getAbbrTitle, getDataInputValue, getImgAreaAlt, getInnerTextWithImgs ] <*> pure e
extractU e =
  asum $ [ getAAreaHref, getImgAudioVideoSourceSrc
         , extractValueClassPattern [extractValueTitle, extractValue]
         , getAbbrTitle, getDataInputValue, getInnerTextRaw ] <*> pure e
extractDt e =
  asum $ (extractValueClassPattern ms : ms ++ [getInnerTextRaw]) <*> pure e
  where ms = [ getTimeInsDelDatetime, getAbbrTitle, getDataInputValue ]

implyProperty ∷ String → Element → Maybe Text
implyProperty "name"  e = asum $ [ getImgAreaAlt, getAbbrTitle
                                 , getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle
                                 , \e' -> asum $ [ getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle ] <*> getOnlyChildren e'
                                 , getInnerTextRaw ] <*> pure e
implyProperty "photo" e = asum $ [ getImgSrc, getObjectData
                                 , getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData
                                 , \e' -> asum $ [ getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData ] <*> getOnlyChildren e'
                                 ] <*> pure e
implyProperty "url"   e = asum $ [ getAAreaHref, getOnlyOfTypeAAreaHref ] <*> pure e
implyProperty _ _ = Nothing
