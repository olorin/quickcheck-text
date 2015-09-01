{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Text.Encoding
                 
import           Test.QuickCheck
import           Test.QuickCheck.Utf8

prop_decodes_without_exception :: Property
prop_decodes_without_exception = forAll utf8BS $ \bs ->
  decodeUtf8 bs === decodeUtf8 bs

prop_oneByte_lsb :: Property
prop_oneByte_lsb = forAll oneByte $ \bs ->
  let b = head $ BS.unpack bs
  in testBit b 7 === False

return []
main = $quickCheckAll
