{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Image.LibPNG where

import Foreign
import Foreign.C.String
import Control.Monad
import Data.List

newtype PNG = PNG (Ptr ())

data PNGImage = PNGImage
    { widthImage :: Word
    , heightImage :: Word
    , rowsImage :: [PNGRow] }

newtype PNGRow = PNGRow
    { foreignPtrRow :: ForeignPtr Word8 }

newtype Pixel = Pixel ( Word8, Word8, Word8, Word8 )
    deriving ( Eq, Ord, Show )

blackPixel :: Pixel
blackPixel = Pixel ( 0, 0, 0, 255 )

whitePixel :: Pixel
whitePixel = Pixel ( 255, 255, 255, 255 )

redPixel :: Pixel
redPixel = Pixel ( 255, 255, 0, 0 )

greenPixel :: Pixel
greenPixel = Pixel ( 255, 0, 255, 0 )

bluePixel :: Pixel
bluePixel = Pixel ( 255, 0, 0, 255 )

greyPixel :: Word8 -> Word8 -> Pixel
greyPixel a x = Pixel ( x, x, x, a )

colorPixel :: Word8 -> Word8 -> Word8 -> Word8 -> Pixel
colorPixel a r g b = Pixel ( r, g, b, a )

listFromPixel :: Pixel -> [Word8]
listFromPixel (Pixel ( r, g, b, a )) = [ r, g, b, a ]

rowPtrFromPixels :: [Pixel] -> IO (Ptr Word8)
rowPtrFromPixels pixels = newArray $ concatMap listFromPixel pixels

-- using a foreign pointer for row
wrapRowPtr :: Ptr Word8 -> IO PNGRow
wrapRowPtr rowp = liftM PNGRow $ newForeignPtr finalizerFree rowp

-- combine a list of pixels into a binary row
rowFromPixels :: [Pixel] -> IO PNGRow
rowFromPixels pixels = rowPtrFromPixels pixels >>= wrapRowPtr

-- new uninitialized binary row
rowEmpty :: Word -> IO PNGRow
rowEmpty width = mallocArray (fromIntegral $ 4*width) >>= wrapRowPtr

pixelFromList :: [Word8] -> Maybe ( Pixel, [Word8] )
pixelFromList ( r : g : b : a : ws ) = Just ( Pixel ( r, g, b, a ), ws )
pixelFromList _ = Nothing

pixelsFromList :: [Word8] -> [Pixel]
pixelsFromList = unfoldr pixelFromList

pixelsFromRowPtr :: Word -> Ptr Word8 -> IO [Pixel]
pixelsFromRowPtr width rowp = do
    ws <- peekArray (fromIntegral $ 4*width) rowp
    return $ pixelsFromList ws

-- extract a list of pixels from a binary row
pixelsFromRow :: Word -> PNGRow -> IO [Pixel]
pixelsFromRow width (PNGRow rowfp) = withForeignPtr rowfp (pixelsFromRowPtr width)

-- construct a image struct from a list of list of pixels
imageFromPixelss :: [[Pixel]] -> IO PNGImage
imageFromPixelss pixelss = do
    rows <- mapM rowFromPixels pixelss'
    return $ PNGImage
        { widthImage = fromIntegral len
        , heightImage = fromIntegral $ length pixelss
        , rowsImage = rows }
  where
    len = length . head $ pixelss
    pixelss' = map (take len) . map (++ repeat whitePixel) $ pixelss

-- extract a list of list of pixels from a image
pixelssFromImage :: PNGImage -> IO [[Pixel]]
pixelssFromImage image = mapM ( pixelsFromRow width ) rows where
    width = widthImage image
    rows = rowsImage image

c_PNG_LIBPNG_VER_STRING :: CString
c_PNG_LIBPNG_VER_STRING = unsafePerformIO $ newCString "1.2.44"

c_PNG_INTERLACE_NONE :: Int
c_PNG_INTERLACE_NONE = 0

c_PNG_COMPRESSION_TYPE_BASE :: Int
c_PNG_COMPRESSION_TYPE_BASE = 0

c_PNG_FILTER_TYPE_BASE :: Int
c_PNG_FILTER_TYPE_BASE = 0

foreign import ccall "png.h png_create_read_struct"
    c_png_create_read_struct :: CString -> Ptr () -> Ptr () -> Ptr ()
                             -> IO PNG

foreign import ccall "png.h png_create_write_struct"
    c_png_create_write_struct :: CString -> Ptr () -> Ptr () -> Ptr ()
                              -> IO PNG

type CFile = Ptr ()

foreign import ccall "png.h png_init_io"
    c_png_init_io :: PNG -> CFile -> IO ()

foreign import ccall "stdio.h fopen"
    c_fopen :: CString -> CString -> IO CFile

foreign import ccall "stdio.h fclose"
    c_fclose :: CFile -> IO ()

type PNGInfo = Ptr ()

foreign import ccall "png.h png_create_info_struct"
    c_png_create_info_struct :: PNG -> IO PNGInfo

foreign import ccall "png.h png_set_IHDR"
    c_png_set_IHDR :: PNG -> PNGInfo -> Word -> Word
                   -> Int -> Int -> Int -> Int -> Int
                   -> IO ()

foreign import ccall "png.h png_get_IHDR"
    c_png_get_IHDR :: PNG -> PNGInfo -> Ptr Word -> Ptr Word
                   -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int
                   -> IO ()

foreign import ccall "png.h png_write_info"
    c_png_write_info :: PNG -> PNGInfo -> IO ()

foreign import ccall "png.h png_read_info"
    c_png_read_info :: PNG -> PNGInfo -> IO ()

foreign import ccall "png.h png_read_update_info"
    c_png_read_update_info :: PNG -> PNGInfo -> IO ()

foreign import ccall "png.h png_get_image_width"
    c_png_get_image_width :: PNG -> PNGInfo -> IO Word

foreign import ccall "png.h png_get_image_height"
    c_png_get_image_height :: PNG -> PNGInfo -> IO Word

type RowPtrPointer = Ptr (Ptr Word8)

foreign import ccall "png.h png_write_image"
    c_png_write_image :: PNG -> RowPtrPointer -> IO ()

foreign import ccall "png.h png_read_image"
    c_png_read_image :: PNG -> RowPtrPointer -> IO ()

foreign import ccall "png.h png_read_end"
    c_png_read_end :: PNG -> Ptr () -> IO ()

foreign import ccall "png.h png_write_end"
    c_png_write_end :: PNG -> Ptr () -> IO ()

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs [] action = action []
withForeignPtrs (x:xs) action =
    withForeignPtr x $ \x' ->
    withForeignPtrs xs $ \xs' -> action (x':xs')

mode_wb :: CString
mode_wb = unsafePerformIO $ newCString "wb"

mode_rb :: CString
mode_rb = unsafePerformIO $ newCString "rb"

--
writePNGImage :: String -> PNGImage -> IO ()
writePNGImage pngFilename image =
    withCString pngFilename $ \c_png_filename -> do
    file <- c_fopen c_png_filename mode_wb
    png <- c_png_create_write_struct
        c_PNG_LIBPNG_VER_STRING nullPtr nullPtr nullPtr
    info <- c_png_create_info_struct png
    c_png_init_io png file
    let width = widthImage image
        height = heightImage image
        rows = rowsImage image
    c_png_set_IHDR png info width height 8 6
        c_PNG_INTERLACE_NONE c_PNG_COMPRESSION_TYPE_BASE c_PNG_FILTER_TYPE_BASE
    c_png_write_info png info
    withForeignPtrs (map foreignPtrRow rows) $
        \rowps -> withArray rowps $
        \rowpp -> c_png_write_image png rowpp
    c_png_write_end png nullPtr
    c_fclose file

--
readPNGImage :: String -> IO PNGImage
readPNGImage pngFilename = withCString pngFilename $ \c_png_filename -> do
    file <- c_fopen c_png_filename mode_rb
    png <- c_png_create_read_struct
        c_PNG_LIBPNG_VER_STRING nullPtr nullPtr nullPtr
    info <- c_png_create_info_struct png
    c_png_init_io png file
    c_png_read_info png info
    width <- c_png_get_image_width png info
    height <- c_png_get_image_height png info
    rows <- replicateM (fromIntegral $ height) (rowEmpty width)
    withForeignPtrs (map foreignPtrRow rows) $
        \rowps -> withArray rowps $
        \rowpp -> c_png_read_image png rowpp
    c_png_read_end png nullPtr
    c_fclose file
    return PNGImage
        { widthImage = width
        , heightImage = height
        , rowsImage = rows }

--
writePNGFromPixelss :: String -> [[Pixel]] -> IO ()
writePNGFromPixelss pngFilename pixelss = do
    image <- imageFromPixelss pixelss
    writePNGImage pngFilename image

--
readPixelssFromPNG :: String -> IO [[Pixel]]
readPixelssFromPNG pngFilename = do
    image <- readPNGImage pngFilename
    pixelssFromImage image
