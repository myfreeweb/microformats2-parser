{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE CPP, RankNTypes #-}

module Data.Microformats2.Parser.Internal where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import           Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Foldable (asum)
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Maybe
import           Data.Time.Format
import           Text.XML.Lens hiding (re)
import           Text.HTML.SanitizeXSS
import           Text.Blaze
import           Text.Blaze.Renderer.Text
import           Text.Regex.PCRE.Heavy
import           Safe (readMay)

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

unwrapName ∷ (Name, a) → (Text, a)
unwrapName (Name n _ _, val) = (n, val)

entireFiltered ∷ (Node → Bool) → Traversal' Element Element
entireFiltered pr f e@(Element _ _ ns) = com <$> f e <*> traverse (_Element (entireNotMicroformat f)) (filter pr ns)
  where com (Element n a _) = Element n a

entireNotMicroformat ∷ Traversal' Element Element
entireNotMicroformat = entireFiltered notMf
  where notMf (NodeElement (Element _ a _)) = not $ (fromMaybe "" $ lookup "class" $ map unwrapName $ M.toList a) ≈ [re|h-\w+|]
        notMf _ = True

entireNotClass ∷ Text → Traversal' Element Element
entireNotClass c = entireFiltered notMf
  where notMf (NodeElement (Element _ a _)) = not $ T.isInfixOf c $ fromMaybe "" $ lookup "class" $ map unwrapName $ M.toList a
        notMf _ = True

hasOneClass ∷ [String] → Traversal' Element Element
hasOneClass ns = attributeSatisfies "class" $ \a → any (\x → T.isInfixOf (T.pack x) a) ns

hasClass ∷ String →  Traversal' Element Element
hasClass n = attributeSatisfies "class" $ T.isInfixOf . T.pack $ n

getOnlyChildren ∷ Element → [Element]
getOnlyChildren e = if lengthOf plate e == 1 then e ^.. plate else []

getOnlyChild ∷ Name → Element → Maybe Element
getOnlyChild n e = if' (lengthOf plate e == 1) $ e ^? plate . el n

getOnlyOfType ∷ Name → Element → Maybe Element
getOnlyOfType n e = if' (lengthOf (plate . el n) e == 1) $ e ^? plate . el n

els ∷ [Name] → Traversal' Element Element
els ns f s = if elementName s `elem` ns then f s else pure s

removeWhitespace ∷ Text → Text
removeWhitespace = gsub [re|(\s+|&nbsp;)|] (" " ∷ String) -- lol vim |||||||

getPrism ∷ Prism' Node Text → Element → Maybe Text
getPrism t e = Just . T.strip <$> T.concat $ e ^.. nodes . traverse . t

_InnerHtml ∷ Prism' Node Text
_InnerHtml = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → Just . TL.toStrict . renderMarkup . toMarkup $ e
  _ → Nothing

getAllHtml ∷ Element → Maybe Text
getAllHtml = getPrism _InnerHtml

-- XXX: https://github.com/yesodweb/haskell-xss-sanitize/issues/11
safeTagName ∷ Text → Bool
safeTagName = (`S.member` S.fromList [ "a", "b", "abbr", "acronym", "br", "ul", "li", "ol", "span", "strong", "em",
                                       "i", "q", "img", "time", "strike", "kbd", "dl", "dt", "pre", "p", "blockquote",
                                       "code", "cite", "figure", "figcaption", "big", "dfn" ])

sanitizeAttrs ∷ Element → Element
sanitizeAttrs e = e { elementAttributes = M.fromList $ map wrapName $ mapMaybe modify $ M.toList $ elementAttributes e }
  where modify (Name n _ _, val) = sanitizeAttribute (n, val)
        wrapName (n, val) = (Name n Nothing Nothing, val)

_InnerHtmlSanitized ∷ Prism' Node Text
_InnerHtmlSanitized = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → if not $ safeTagName $ nameLocalName (elementName e)
                       then Nothing
                       else Just . TL.toStrict . renderMarkup . toMarkup $ sanitizeAttrs e
  _ → Nothing

getAllHtmlSanitized ∷ Element → Maybe Text
getAllHtmlSanitized = getPrism _InnerHtmlSanitized

_InnerText ∷ Prism' Node Text
_InnerText = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → if nameLocalName (elementName e) == "img"
                       then e ^. el "img" . attribute "alt"
                       else Just . removeWhitespace . TL.toStrict . renderMarkup . contents . toMarkup $ e
  _ → Nothing

getAllText ∷ Element → Maybe Text
getAllText = getPrism _InnerText

getText ∷ Element → Maybe Text
getText e = if T.null $ fromMaybe "" txt then Nothing else txt
  where txt = listToMaybe $ T.strip <$> e ^.. entire . nodes . traverse . _Content

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
extractValue e = asum $ [ getAbbrTitle, getDataInputValue, getImgAreaAlt, getAllText ] <*> pure e

extractValueTitle ∷ Element → Maybe Text
extractValueTitle e = if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"

extractValueClassPattern ∷ [Element → Maybe Text] → Element → Maybe Text
extractValueClassPattern fs e = if' (isJust $ e ^? valueParts) extractValueParts
  where extractValueParts   = Just . T.concat . catMaybes $ e ^.. valueParts . to extractValuePart
        extractValuePart e' = asum $ fs <*> pure e'
        valueParts          ∷ Applicative f => (Element → f Element) → Element → f Element
        valueParts          = entire . hasOneClass ["value", "value-title"]

className ∷ PropType → String → String
className P  n = "p-" ++ n
className U  n = "u-" ++ n
className Dt n = "dt-" ++ n
className E  n = "e-" ++ n

findProperty ∷ Element → String → [Element]
findProperty e n = filter (/= e) $ e ^.. entireNotMicroformat . hasClass n

findPropertyMicroformat ∷ Element → String → String → [Element]
findPropertyMicroformat e n s = filter (/= e) $ e ^.. entire . hasClass n . hasClass s

findPropertyMicroformatNotNestedIn ∷ Text → Element → String → String → [Element]
findPropertyMicroformatNotNestedIn excl e n s = filter (/= e) $ e ^.. entireNotClass excl . hasClass n . hasClass s

data PropType = P | U | Dt | E

extract ∷ [Element → Maybe Text] → Element → [Text]
extract ps e = catMaybes $ [ \x -> asum $ ps <*> pure x ] <*> pure e

extractProperty ∷ PropType → String → Element → [Text]
extractProperty P n e' =
  findProperty e' (className P n) >>=
  extract [ extractValueClassPattern [extractValueTitle, extractValue]
          , extractValue ]
extractProperty U n e' =
  findProperty e' (className U n) >>=
  extract [ getAAreaHref, getImgAudioVideoSourceSrc
          , extractValueClassPattern [extractValueTitle, extractValue]
          , getAbbrTitle, getDataInputValue, getAllText ]
extractProperty Dt n e' =
  findProperty e' (className Dt n) >>=
  extract (extractValueClassPattern ms : ms ++ [getAllText])
  where ms = [ getTimeInsDelDatetime, getAbbrTitle, getDataInputValue ]
extractProperty E n e' = findProperty e' (className E n) >>= liftM maybeToList getAllHtml

extractPropertyL ∷ PropType → String → Element → [TL.Text]
extractPropertyL t n e = TL.fromStrict <$> if null extracted then maybeToList $ implyProperty t n e else extracted
  where extracted = extractProperty t n e

extractPropertyR ∷ Read α ⇒ PropType → String → Element → [α]
extractPropertyR t n e = catMaybes $ readMay <$> T.unpack <$> extractProperty t n e

extractPropertyDt ∷ ParseTime α ⇒ String → Element → [α]
extractPropertyDt n e = catMaybes $ readISO <$> T.unpack <$> extractProperty Dt n e
  where readISO x = asum $ map ($ x) [ isoParse $ Just "%H:%M:%S%z"
                                     , isoParse $ Just "%H:%M:%SZ"
                                     , isoParse $ Just "%H:%M:%S"
                                     , isoParse $ Just "%H:%M"
                                     , parseTimeD "%G-W%V-%u"
                                     , parseTimeD "%G-W%V"
                                     , isoParse Nothing ]
        isoParse = parseTimeD . iso8601DateFormat
        parseTimeD = parseTimeM True defaultTimeLocale 

extractPropertyContent ∷ (Element → Maybe Text) → PropType → String → Element → [TL.Text]
extractPropertyContent ex t n e = findProperty e (className t n) >>= extract [ ex ] >>= return . TL.fromStrict

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
