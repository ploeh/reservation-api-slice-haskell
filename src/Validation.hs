{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Validation where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

newtype Validated e a =
  Validated (Either e a)
  deriving ( Eq
           , Show
           , Bifunctor
           , Functor
           , Bifoldable
           , Foldable)

instance Bitraversable Validated where
  bitraverse f g (Validated x) = Validated <$> bitraverse f g x

instance Traversable (Validated e) where
  sequenceA = bisequenceA . first pure

instance Semigroup s => Applicative (Validated s) where
  pure = Validated . pure
  Validated (Left x) <*> Validated (Left y) = Validated $ Left $ x <> y
  Validated f <*> Validated x = Validated (f <*> x)
