module Main where

import Test.Framework (defaultMain)

import InterpString.Test

main :: IO ()
main = defaultMain [interpSuite]
