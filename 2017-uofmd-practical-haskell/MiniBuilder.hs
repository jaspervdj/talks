module MiniBuilder
    ( MiniBuilder
    , toByteString

    , string
    , char
    ) where

import           Data.Bits                (shiftR, (.&.))
import qualified Data.ByteString.Internal as B
import           Data.Char                (ord)
import           Data.Word                (Word8)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe         (unsafeDupablePerformIO)

data MiniBuilder = MiniBuilder !Int !(Ptr Word8 -> IO (Ptr Word8))

instance Monoid MiniBuilder where
    mempty  = empty
    mappend = append

toByteString :: MiniBuilder -> B.ByteString
toByteString (MiniBuilder maxLen f) = unsafeDupablePerformIO $ do
    fptr <- mallocForeignPtrArray maxLen
    len  <- withForeignPtr fptr $ \ptr0 -> do
        ptr1 <- f ptr0
        return $ ptr1 `minusPtr` ptr0
    return $ B.fromForeignPtr fptr 0 len

empty :: MiniBuilder
empty = MiniBuilder 0 (\ptr -> return ptr)

append :: MiniBuilder -> MiniBuilder -> MiniBuilder
append (MiniBuilder maxLen0 f) (MiniBuilder maxLen1 g) = MiniBuilder
    (maxLen0 + maxLen1)
    (\ptr0 -> do
        ptr1 <- f ptr0
        g ptr1)

string :: String -> MiniBuilder
string = mconcat . map char

char :: Char -> MiniBuilder
char c
    | x <= 0x7F = MiniBuilder 1 $ \ptr -> do
        poke ptr (fromIntegral x)
        return (ptr `plusPtr` 1)

    | x <= 0x07FF = MiniBuilder 2 $ \ptr -> do
        let b0 = fromIntegral $ (x `shiftR` 6) + 0xC0 :: Word8
            b1 = fromIntegral $ (x .&. 0x3F)   + 0x80 :: Word8

        poke ptr               b0
        poke (ptr `plusPtr` 1) b1
        return (ptr `plusPtr` 2)

    | x <= 0xFFFF = MiniBuilder 3 $ \ptr -> do
        let b0 = fromIntegral $ (x `shiftR` 12) + 0xE0           :: Word8
            b1 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80 :: Word8
            b2 = fromIntegral $ (x .&. 0x3F) + 0x80              :: Word8

        poke ptr               b0
        poke (ptr `plusPtr` 1) b1
        poke (ptr `plusPtr` 2) b2
        return (ptr `plusPtr` 3)

    | otherwise = MiniBuilder 4 $ \ptr -> do
        let b0 = fromIntegral $ (x `shiftR` 18) + 0xF0            :: Word8
            b1 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80 :: Word8
            b2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80  :: Word8
            b3 = fromIntegral $ (x .&. 0x3F) + 0x80               :: Word8

        poke ptr               b0
        poke (ptr `plusPtr` 1) b1
        poke (ptr `plusPtr` 2) b2
        poke (ptr `plusPtr` 3) b3
        return (ptr `plusPtr` 4)
  where
    x = ord c :: Int
