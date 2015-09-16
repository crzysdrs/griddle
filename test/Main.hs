module Main where

import Test.Framework (defaultMain)

import InterpString.Test
import Steam.BinVDF.Test

main :: IO ()
main = defaultMain [interpSuite, binvdfSuite]
