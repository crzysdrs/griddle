module InterpString.Test where

import Control.Monad
import InterpString
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

genInterpPlain :: Gen String
genInterpPlain = do
  e <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']
  brackets <- listOf1 $ elements ["\\}", "\\{"]
  shuffled <- shuffle (map (\x -> [x]) e ++ brackets)
  return $ concat shuffled

instance Arbitrary InterpNode where
    arbitrary = oneof
                [
                 liftM InterpLookup genInterpPlain,
                 liftM2 InterpCmd genInterpPlain arbitrary
                ]
instance Arbitrary InterpString where
    arbitrary = oneof
                [
                 liftM InterpPlain genInterpPlain,
                 liftM2 InterpJoin arbitrary arbitrary
                ]

interpSuite :: Test
interpSuite = testGroup "InterpSuite"
   [ testProperty "interpString Rount Trip" prop_interpStringRoundTrip ]

prop_interpStringRoundTrip :: InterpString -> Bool
prop_interpStringRoundTrip is = parseInterpString (deparseInterpString is) == is
