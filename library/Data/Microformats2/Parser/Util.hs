{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}

module Data.Microformats2.Parser.Util where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Text.Regex.PCRE.Heavy

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

unless' ∷ Bool → Maybe a → Maybe a
unless' c x = if c then Nothing else x

listToMaybeList ∷ [α] → Maybe [α]
listToMaybeList l = unless' (null l) $ Just l

stripQueryString ∷ TL.Text → TL.Text
stripQueryString = TL.intercalate "" . take 1 . TL.splitOn "?" . TL.strip

removeWhitespace ∷ T.Text → T.Text
removeWhitespace = gsub [re|(\s+|&nbsp;)|] (" " ∷ String)
