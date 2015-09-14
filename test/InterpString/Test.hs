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
instance Arbitrary InterpTop where
    arbitrary = oneof
                [
                 liftM InterpStr genInterpPlain,
                 liftM InterpElem arbitrary
                ]

mergeString :: InterpString -> InterpString
mergeString ((InterpStr s1):(InterpStr s2):xs) = mergeString (newStr:xs)
    where newStr = (InterpStr (s1++s2))
mergeString (x:xs) = x:(mergeString xs)
mergeString [] = []

interpSuite :: Test
interpSuite = testGroup "InterpSuite"
   [ testProperty "interpString Round Trip" prop_interpStringRoundTrip ]

prop_interpStringRoundTrip :: InterpString -> Bool
prop_interpStringRoundTrip is = parseInterpString (deparseInterpString is) == mergeString is
