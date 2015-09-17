module InterpString.Test where

import Control.Monad
import InterpString
import qualified Data.HashMap.Strict as HM
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
   [
    testProperty "interpString Round Trip" $
      \is -> parseInterpString (deparseInterpString is) == mergeString is,
    testProperty "interpString Generate" prop_genInterpString
   ]

interpKeys :: HM.HashMap String String
interpKeys = HM.fromList [("test", "123"), ("other", "576")]
interpFuncs :: HM.HashMap String (String -> String)
interpFuncs = HM.fromList [("id", id), ("xout", map (\x -> 'x'))]

validInterpString :: Gen InterpString
validInterpString = do
  is <- arbitrary
  mapM validInterpTop is

validInterpTop :: InterpTop -> Gen InterpTop
validInterpTop (InterpElem e) = do
  newE <- validInterpNode e
  return $ InterpElem newE
validInterpTop x = return $ x

validInterpNode :: InterpNode -> Gen InterpNode
validInterpNode (InterpCmd _ n) = do
  k <- oneof $ map return $ HM.keys interpFuncs
  newN <- validInterpNode n
  return $ InterpCmd k newN
validInterpNode (InterpLookup _) = do
  l <- oneof $ map return $ HM.keys interpKeys
  return $ InterpLookup l

prop_genInterpString = forAll validInterpString $ \is ->
    case convertInterpString interpKeys interpFuncs is of
      Right r -> True
      Left l -> False
