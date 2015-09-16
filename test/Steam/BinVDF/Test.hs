module Steam.BinVDF.Test where

import Data.Aeson
import Control.Monad
import Steam.BinVDF
import Data.Maybe
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Vector as V
import qualified Data.Text as T

genScientificInt :: Gen Scientific
genScientificInt = do
  i <- arbitrary
  return $ scientific i 0

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = do
    v <- listOf arbitrary
    return $ V.fromList v

containsNull :: T.Text -> Bool
containsNull s = or $ map ((==) '\NUL') (T.unpack s)

textNull :: Value -> Bool
textNull (Object h) =  do
  let (ks, vs) = unzip $ map (\(k,v) -> (containsNull k, textNull v)) $ HM.toList h
  or ks || or vs
textNull (String s) = containsNull s
textNull (Number _) = False
textNull _ = undefined

instance Arbitrary Value where
  arbitrary = oneof [
      liftM Object arbitrary,
      liftM String arbitrary,
      liftM Number genScientificInt
      ]

isObj :: Value -> Bool
isObj (Object _) = True
isObj _ = False

binvdfSuite :: Test
binvdfSuite = testGroup "binvdfSuite"
  [
    testProperty "binVDF Round Trip" prop_binvdfRoundTrip
  ]

prop_binvdfRoundTrip :: Value -> Property
prop_binvdfRoundTrip v = not (textNull v) && isObj v ==>
  fromMaybe (error "Invalid JSON") (vdfJSON "ignore" (jsonVDF v)) == v
