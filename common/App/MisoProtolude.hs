{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.MisoProtolude () where

import Miso.String
import Protolude

instance (ToMisoString a) => StringConv a MisoString where
  strConv _ = toMisoString

instance (FromMisoString a) => StringConv MisoString a where
  strConv _ = fromMisoString
