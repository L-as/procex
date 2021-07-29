{-# OPTIONS_GHC -Wno-orphans #-}

module Procex.Shell.Labels where

import Data.Functor
import Data.Proxy (Proxy (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

instance (a ~ String, KnownSymbol l) => IsLabel l a where
  fromLabel =
    symbolVal (Proxy :: Proxy l) <&> \case
      '_' -> '-'
      x -> x
