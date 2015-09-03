module Test.QuickCheck.Utf8(
    genValidUtf8
  , utf8BS
    -- * Generators for single characters
  , oneByte
  , twoByte
  , threeByte
  , fourByte
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

genValidUtf8 :: Gen Text
genValidUtf8 = fmap decodeUtf8 utf8BS

utf8BS :: Gen ByteString
utf8BS = fmap BS.concat $ elements symbolTypes >>= listOf

symbolTypes :: [Gen ByteString]
symbolTypes = [ oneByte
              , twoByte
              , threeByte
              , fourByte
              ]

inRange :: Int -> Int -> Gen Word8
inRange lo hi = fmap fromIntegral $ elements [lo..hi]

-- | Single-byte UTF-8 (i.e., a standard ASCII byte with a cleared MSB).
oneByte :: Gen ByteString
oneByte = fmap (BS.pack . pure) $
  inRange 0 127 -- 0bbbbbbb

twoByte :: Gen ByteString
twoByte = do
  b1 <- inRange 192 223 -- 110bbbbb
  b2 <- nonInitial
  pure . buildUtf $ putBytes2 b1 b2

threeByte :: Gen ByteString
threeByte = do
  b1 <- inRange 224 239 -- 1110bbbb
  (b2, b3) <- fmap (,) nonInitial `ap` nonInitial
  pure . buildUtf $ putBytes3 b1 b2 b3

-- | 
fourByte :: Gen ByteString
fourByte = do
  -- Per <https://tools.ietf.org/html/rfc3629 RFC 3629>, the four-byte
  -- range ends at U+10FFF.
  b1 <- inRange 0 1
  (b2, b3, b4) <- fmap (,,) nonInitial `ap` nonInitial `ap` nonInitial
  pure . buildUtf $ putBytes4 b1 b2 b3 b4

buildUtf :: Builder -> ByteString 
buildUtf = BS.concat . BL.toChunks . toLazyByteString

putBytes2 :: Word8 -> Word8 -> Builder
putBytes2 b1 b2 =  putCharUtf8 . chr . fromIntegral . runGet getWord16be $ BL.pack [b1, b2]

putBytes3 :: Word8 -> Word8 -> Word8 -> Builder
putBytes3 b1 b2 b3 =  putCharUtf8 . chr . runGet getWord24be $ BL.pack [b1, b2, b3]
 where
  getWord24be :: Get Int
  getWord24be = do
    w <- fromIntegral <$> getWord16be
    b <- fromIntegral <$> getWord8
    pure $ w + b

putBytes4 :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
putBytes4 b1 b2 b3 b4 =  putCharUtf8 . chr . fromIntegral . runGet getWord32be $ BL.pack [b1, b2, b3, b4]

nonInitial :: Gen Word8
nonInitial = inRange 128 191
