module Data.Text.Arbitrary(
  Text()
) where

import          Data.Text

import          Test.QuickCheck
import          Test.QuickCheck.Text

instance Arbitrary Text where
  arbitrary = genValidUtf8
