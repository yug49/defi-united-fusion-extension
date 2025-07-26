{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_cross_chain_swap_cardano (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "cross_chain_swap_cardano"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "description:       A extension of 1inch for cross-chain swaps on the Cardano blockchain."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
