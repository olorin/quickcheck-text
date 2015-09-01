module Data.Text.Arbitrary(
  Text()
) where

import          Data.Text
import          Test.QuickCheck

instance Arbitrary Text where
  arbitrary = undefined
