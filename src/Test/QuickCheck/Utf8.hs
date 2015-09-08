module Test.QuickCheck.Utf8(
    genValidUtf8
  , utf8BS
  , genValidUtf81
  , utf8BS1
    -- * Generators for single characters
  , oneByte
  , twoByte
  , threeByte
) where

import           Control.Monad

import           Data.Binary.Builder
import           Data.Binary.Get

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.Char
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding
import           Data.Word

import           Test.QuickCheck

-- |
-- Generate a possibly-empty valid UTF-8 'Text' value.
genValidUtf8 :: Gen Text
genValidUtf8 = fmap decodeUtf8 utf8BS

-- |
-- Generate a possibly-empty sequence of bytes which represent a valid
-- UTF-8 code point.
utf8BS :: Gen ByteString
utf8BS = fmap BS.concat $ elements symbolTypes  >>= listOf

-- |
-- Like 'genValidUtf8', but does not allow empty 'Text' values.
genValidUtf81 :: Gen Text
genValidUtf81 = fmap decodeUtf8 utf8BS1

-- |
-- Like 'utf8BS', but does not allow empty 'ByteString's.
utf8BS1 :: Gen ByteString
utf8BS1 = fmap BS.concat $ elements symbolTypes  >>= listOf1

symbolTypes :: [Gen ByteString]
symbolTypes = [ oneByte
              , twoByte
              , threeByte
              ]

inRange :: Int -> Int -> Gen Word8
inRange lo hi = fmap fromIntegral $ elements [lo..hi]

-- | Single-byte UTF-8 (i.e., a standard ASCII byte with a cleared MSB).
oneByte :: Gen ByteString
oneByte = fmap (BS.pack . return) $
  inRange 0 127 -- 0bbbbbbb

twoByte :: Gen ByteString
twoByte = do
  b1 <- inRange 192 223 -- 110bbbbb
  b2 <- nonInitial
  return . buildUtf $ putBytes2 b1 b2

threeByte :: Gen ByteString
threeByte = do
  b1 <- inRange 224 239 -- 1110bbbb
  (b2, b3) <- fmap (,) nonInitial `ap` nonInitial
  return . buildUtf $ putBytes3 b1 b2 b3

buildUtf :: Builder -> ByteString 
buildUtf = BS.concat . BL.toChunks . toLazyByteString

putBytes2 :: Word8 -> Word8 -> Builder
putBytes2 b1 b2 =  putCharUtf8 . chr . fromIntegral . runGet getWord16be $ BL.pack [b1, b2]

putBytes3 :: Word8 -> Word8 -> Word8 -> Builder
putBytes3 b1 b2 b3 =  putCharUtf8 . chr . runGet getWord24be $ BL.pack [b1, b2, b3]
 where
  getWord24be :: Get Int
  getWord24be = do
    w <- fromIntegral `fmap` getWord16be
    b <- fromIntegral `fmap` getWord8
    return $ w + b

nonInitial :: Gen Word8
nonInitial = inRange 128 191
