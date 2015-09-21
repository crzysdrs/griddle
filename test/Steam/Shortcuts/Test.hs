module Steam.Shortcuts.Test where

import Steam.Shortcuts
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Data.Maybe

instance (Arbitrary a) => Arbitrary (VDFList a) where
  arbitrary = do
    l <- listOf arbitrary
    return $ VDFList l

instance Arbitrary SteamShortcut where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    sd <- arbitrary
    i <- arbitrary
    t <- arbitrary
    h <- arbitrary
    p <- arbitrary
    return $ SteamShortcut {
      appName = a,
      exe = e,
      startDir = sd,
      icon = i,
      tags = t,
      hidden = h,
      path = p
      }

shortcutSuite :: Test
shortcutSuite = testGroup "shortcutSuite"
  [
    testProperty "Shortcuts Round Trip" prop_shortcutRoundTrip
  ]

prop_shortcutRoundTrip :: [SteamShortcut] -> Bool
prop_shortcutRoundTrip shorts = fromJust (parseShortcuts (deparseShortcuts shorts)) == shorts
