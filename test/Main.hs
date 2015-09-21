module Main where

import Test.Framework (defaultMain)

import InterpString.Test
import Steam.BinVDF.Test
import Steam.Shortcuts.Test

main :: IO ()
main = defaultMain [interpSuite, binvdfSuite, shortcutSuite]
