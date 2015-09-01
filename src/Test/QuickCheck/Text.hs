module Test.QuickCheck.Text(
  genValidUtf8
) where

import           Control.Applicative

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Text as T

import           Test.QuickCheck

genValidUtf8 :: Gen Text
genValidUtf8 = fmap decodeUtf8 utf8BS

utf8BS :: Gen ByteString
utf8BS = fmap BS.concat $ elements symbolTypes >>= listOf

symbolTypes :: [Gen ByteString]
symbolTypes = [ oneByte
              ]

-- | Single-byte UTF-8 (i.e., a standard ASCII byte with a cleared MSB).
oneByte :: Gen ByteString
oneByte = fmap (BS.pack . pure . fromIntegral)  $ elements [0..127]


