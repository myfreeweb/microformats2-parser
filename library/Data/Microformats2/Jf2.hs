{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, OverloadedStrings #-}

module Data.Microformats2.Jf2 (mf2ToJf2) where

import           Prelude.Compat
import           Data.Maybe
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Microformats2.Parser.UnsafeUtil

mf2ToJf2 ∷ Value → Value
mf2ToJf2 val@(Object _) = flattenItems $ processItems $ processChildren $ flattenProps $ val & key "type" %~ flattenType
  where flattenType t@(Array _) = fromMaybe Null $ fmap (& _String %~ (\x → fromMaybe x $ T.stripPrefix "h-" x)) $ t ^? nth 0
        flattenType x = x
        flattenProps p@(Object o) = Object $ HMS.delete "value" $ HMS.delete "properties" $
          foldl (\acc (k, v) → HMS.insert k (flattenArr $ processProp k v) acc) o $
          p ^@.. key "properties" . members
        flattenProps x = x
        processProp "content" v = v & _Array . each %~ processContent
        processProp _ v = v & _Array . each %~ mf2ToJf2
        processContent (Object o) =
          case HMS.lookup "value" o of
               Just v → Object $ HMS.delete "value" $ HMS.insert "text" v o
               _ → Object o
        processContent x = x
        flattenArr (Array v) | V.length v == 1 = V.head v
        flattenArr x = x
        processChildren x = x & key "children" . _Array . each %~ mf2ToJf2
        processItems x = x & key "items" . _Array . each %~ mf2ToJf2
        flattenItems x | x ^? key "items" . _Array . to V.length == Just 1 = fromMaybe x $ x ^? key "items" . nth 0
        flattenItems x = x
mf2ToJf2 x = x & _Array . each %~ mf2ToJf2
