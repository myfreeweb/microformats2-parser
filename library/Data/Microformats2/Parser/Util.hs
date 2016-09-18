{-# LANGUAGE Safe, NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, TupleSections #-}

module Data.Microformats2.Parser.Util (
  module Data.Microformats2.Parser.Util
, module Data.Microformats2.Parser.UnsafeUtil
) where

import           Prelude.Compat
import           Data.Maybe
import           Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import qualified Data.Foldable as F
import           Network.URI
import           Data.Microformats2.Parser.UnsafeUtil

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

unless' ∷ Bool → Maybe a → Maybe a
unless' c x = if c then Nothing else x

listToMaybeList ∷ [α] → Maybe [α]
listToMaybeList l = unless' (null l) $ Just l

stripQueryString ∷ TL.Text → TL.Text
stripQueryString = TL.intercalate "" . take 1 . TL.splitOn "?" . TL.strip

groupBy' ∷ (Ord β) ⇒ (α → β) → [α] → [(β, [α])]
groupBy' f = M.toAscList . M.fromListWith (++) . map (\a → (f a, [a]))

-- https://hackage.haskell.org/package/liquid-fixpoint-0.4.0.0/docs/src/Language-Fixpoint-Misc.html#expandSnd
expandSnd ∷ F.Foldable φ ⇒ φ ([α], β) → [(α, β)]
expandSnd = F.concatMap (\(xs, y) → (, y) <$> xs)

resolveURI ∷ Maybe URI → T.Text → T.Text
resolveURI b t =
  case parseURIReference $ resolveSlashSlash $ T.unpack t of
    Just u → T.pack $ uriToString id (u `relativeTo` baseUri') ""
    Nothing → t
  where resolveSlashSlash x = if "//" `isPrefixOf` x then uriScheme baseUri' ++ x else x
        baseUri' = fromMaybe nullURI b
