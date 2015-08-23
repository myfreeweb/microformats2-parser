{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE CPP, TupleSections #-}

module Data.Microformats2.Parser.Util where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Foldable as F
import           Text.Regex.PCRE.Heavy

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

unless' ∷ Bool → Maybe a → Maybe a
unless' c x = if c then Nothing else x

listToMaybeList ∷ [α] → Maybe [α]
listToMaybeList l = unless' (null l) $ Just l

stripQueryString ∷ TL.Text → TL.Text
stripQueryString = TL.intercalate "" . take 1 . TL.splitOn "?" . TL.strip

collapseWhitespace ∷ T.Text → T.Text
collapseWhitespace = gsub [re|(\s+|&nbsp;)|] (" " ∷ String)

emptyVal ∷ Value → Bool
emptyVal (Object o) = HMS.null o
emptyVal (Array v) = V.null v
emptyVal (String s) = T.null s
emptyVal Null = True
emptyVal _ = False

groupBy' ∷ (Ord β) ⇒ (α → β) → [α] → [(β, [α])]
groupBy' f = M.toAscList . M.fromListWith (++) . map (\a → (f a, [a]))

-- https://hackage.haskell.org/package/liquid-fixpoint-0.4.0.0/docs/src/Language-Fixpoint-Misc.html#expandSnd
expandSnd ∷ F.Foldable φ ⇒ φ ([α], β) → [(α, β)]
expandSnd = F.concatMap (\(xs, y) → (, y) <$> xs)
