{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module contains an instance for labels,
 that causes labels to be interpreted as
 strings, with @_@ replaced with @-@.

 It is useful for shells, since it's
 easier to type arguments with labels.

 NB: @_@ is replaced with @-@.
-}
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
