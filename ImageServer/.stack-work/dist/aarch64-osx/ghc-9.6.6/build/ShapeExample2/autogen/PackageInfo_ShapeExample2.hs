{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ShapeExample2 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ShapeExample2"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Simple shape example for CS4012"
copyright :: String
copyright = "2017 Glenn Strong"
homepage :: String
homepage = "https://github.com/g-d-strong/ShapeExample1#readme"
