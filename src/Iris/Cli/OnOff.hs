{- |
Module                  : Iris.Cli.OnOff
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI options parsing for @--version@ and @--numeric-version@

!TODO Write module description

@since 0.0.0.0
-}


module Iris.Cli.OnOff where

import Data.Functor ((<&>))
import qualified Options.Applicative as Opt

data OnOff
    = Off
    | On

onoff :: Opt.Mod Opt.FlagFields Bool -> Opt.Parser OnOff
onoff mods = Opt.switch mods <&> \case
    False -> Off
    True  -> On
