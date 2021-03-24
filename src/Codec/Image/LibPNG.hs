{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass #-}
module Codec.Image.LibPNG where

import Foreign
import Foreign.C.String
import Control.Monad
import Data.List
import System.IO.Unsafe
import qualified Debug.Trace as Debug
import System.CPUTime (getCPUTime)
import Control.DeepSeq ( NFData(..) )

instance NFData PNGRow where
  rnf r = rnf foreignPtrRow

newtype PNG = PNG (Ptr ())

data PNGImage = PNGImage
    { widthImage :: Word
    , heightImage :: Word
    , depthImage :: Word
    , alphaPosImage :: Word
    , rowsImage :: [PNGRow] }

newtype PNGRow = PNGRow
    { foreignPtrRow :: ForeignPtr Word8 }

newtype Pixel = Pixel ( Word8, Word8, Word8, Word8 )
    deriving ( Eq, Ord, Show )

rgbaDepth :: Word
rgbaDepth = 4

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

-- rowPtrFromPixels :: [Pixel] -> IO (Ptr Word8)
-- rowPtrFromPixels pixels = newArray $ concatMap listFromPixel pixels

-- -- using a foreign pointer for row
wrapRowPtr :: Ptr Word8 -> IO PNGRow
wrapRowPtr rowp = liftM PNGRow $ newForeignPtr finalizerFree rowp

-- -- combine a list of pixels into a binary row
-- rowFromPixels :: [Pixel] -> IO PNGRow
-- rowFromPixels pixels = rowPtrFromPixels pixels >>= wrapRowPtr

-- -- new uninitialized binary row
rowEmpty :: Word -> Word -> IO PNGRow
rowEmpty depth width = mallocArray (fromIntegral $ depth*width) >>= wrapRowPtr

-- pixelFromList :: Word -> [Word8] -> Maybe ( Pixel, [Word8] )
-- pixelFromList 4 ( r : g : b : a : ws ) = Just ( Pixel ( r, g, b, a ), ws )
-- pixelFromList 3 ( r : g : b : ws ) = Just ( Pixel ( r, g, b, 255 ), ws )
-- pixelFromList 2 ( w : a : ws ) = Just ( Pixel ( w, w, w, a ), ws )
-- pixelFromList 1 ( w : ws ) = Just ( Pixel ( w, w, w, 255 ), ws )
-- pixelFromList _ _ = Nothing

-- pixelsFromList :: Word -> [Word8] -> [Pixel]
-- pixelsFromList depth = unfoldr$ pixelFromList depth

-- pixelsFromRowPtr :: Word -> Word -> Ptr Word8 -> IO [Pixel]
-- pixelsFromRowPtr depth width rowp = do
--     ws <- peekArray (fromIntegral $ depth*width) rowp
--     return $ pixelsFromList depth ws

-- -- extract a list of pixels from a binary row
-- pixelsFromRow :: Word -> Word -> PNGRow -> IO [Pixel]
-- pixelsFromRow depth width (PNGRow rowfp) = withForeignPtr rowfp$ pixelsFromRowPtr depth width

-- -- construct a image struct from a list of list of pixels
-- imageFromPixelss :: [[Pixel]] -> IO PNGImage
-- imageFromPixelss pixelss = do
--     rows <- mapM rowFromPixels pixelss'
--     return $ PNGImage
--         { widthImage = fromIntegral len
--         , heightImage = fromIntegral $ length pixelss
--         , depthImage = rgbaDepth
--         , alphaPosImage = rgbaDepth - 1
--         , rowsImage = rows }
--   where
--     len = length . head $ pixelss
--     pixelss' = map (take len) . map (++ repeat whitePixel) $ pixelss

-- -- extract a list of list of pixels from a image
-- pixelssFromImage :: PNGImage -> IO [[Pixel]]
-- pixelssFromImage image = mapM (pixelsFromRow depth width) rows where
--     width = widthImage image
--     rows = rowsImage image
--     depth = depthImage image

c_PNG_LIBPNG_VER_STRING :: CString
c_PNG_LIBPNG_VER_STRING = unsafePerformIO $ newCString "1.6.16"

c_PNG_INTERLACE_NONE :: Int
c_PNG_INTERLACE_NONE = 0

c_PNG_COMPRESSION_TYPE_BASE :: Int
c_PNG_COMPRESSION_TYPE_BASE = 0

c_PNG_FILTER_TYPE_BASE :: Int
c_PNG_FILTER_TYPE_BASE = 0

c_PNG_COLOR_TYPE_GRAY :: Word8
c_PNG_COLOR_TYPE_GRAY = 0

c_PNG_COLOR_TYPE_RGB :: Word8
c_PNG_COLOR_TYPE_RGB = 2

c_PNG_COLOR_TYPE_PALETTE :: Word8
c_PNG_COLOR_TYPE_PALETTE = 3

c_PNG_COLOR_TYPE_GRAY_ALPHA :: Word8
c_PNG_COLOR_TYPE_GRAY_ALPHA = 4

c_PNG_COLOR_TYPE_RGB_ALPHA :: Word8
c_PNG_COLOR_TYPE_RGB_ALPHA = 6

c_PNG_FILLER_AFTER :: Int
c_PNG_FILLER_AFTER = 1

c_PNG_INFO_tRNS :: Word
c_PNG_INFO_tRNS = 0x0010

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

foreign import ccall "png.h png_get_valid"
    c_png_get_valid :: PNG -> PNGInfo -> Word -> IO Word

foreign import ccall "png.h png_get_color_type"
    c_png_get_color_type :: PNG -> PNGInfo -> IO Word8

foreign import ccall "png.h png_get_bit_depth"
    c_png_get_bit_depth :: PNG -> PNGInfo -> IO Word8

foreign import ccall "png.h png_set_expand_gray_1_2_4_to_8"
    c_png_set_expand_gray_1_2_4_to_8 :: PNG -> IO ()

foreign import ccall "png.h png_set_palette_to_rgb"
    c_png_set_palette_to_rgb :: PNG -> IO ()

foreign import ccall "png.h png_set_strip_16"
    c_png_set_strip_16 :: PNG -> IO ()

foreign import ccall "png.h png_set_tRNS_to_alpha"
    c_png_set_tRNS_to_alpha :: PNG -> IO ()

foreign import ccall "png.h png_set_filler"
    c_png_set_filler :: PNG -> Word -> Int -> IO ()

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
-- writePNGImage :: String -> PNGImage -> IO ()
-- writePNGImage pngFilename image =
--     withCString pngFilename $ \c_png_filename -> do
--     file <- c_fopen c_png_filename mode_wb
--     png <- c_png_create_write_struct
--         c_PNG_LIBPNG_VER_STRING nullPtr nullPtr nullPtr
--     info <- c_png_create_info_struct png
--     c_png_init_io png file
--     let width = widthImage image
--         height = heightImage image
--         rows = rowsImage image
--     c_png_set_IHDR png info width height 8 6
--         c_PNG_INTERLACE_NONE c_PNG_COMPRESSION_TYPE_BASE c_PNG_FILTER_TYPE_BASE
--     c_png_write_info png info
--     withForeignPtrs (map foreignPtrRow rows) $
--         \rowps -> withArray rowps $
--         \rowpp -> c_png_write_image png rowpp
--     c_png_write_end png nullPtr
--     c_fclose file

--
readPNGImage :: Bool -> Maybe (Ptr Word8) -> String -> IO (Either PNGImage ())
readPNGImage oneRow outPtr pngFilename = withCString pngFilename $ \c_png_filename -> do
    file <- c_fopen c_png_filename mode_rb
    png <- c_png_create_read_struct
        c_PNG_LIBPNG_VER_STRING nullPtr nullPtr nullPtr
    info <- c_png_create_info_struct png
    c_png_init_io png file
    c_png_read_info png info
    width <- c_png_get_image_width png info
    height <- c_png_get_image_height png info
    color_type <- c_png_get_color_type png info
    bit_depth <- c_png_get_bit_depth png info
    tRNS <- c_png_get_valid png info c_PNG_INFO_tRNS
    when (bit_depth == 16)$ c_png_set_strip_16 png
    when (color_type == c_PNG_COLOR_TYPE_GRAY && bit_depth < 8)$ c_png_set_expand_gray_1_2_4_to_8 png
    when (color_type == c_PNG_COLOR_TYPE_PALETTE)$ c_png_set_palette_to_rgb png
    when (tRNS /= 0)$ c_png_set_tRNS_to_alpha png
    let
      depth
        | color_type == c_PNG_COLOR_TYPE_GRAY = 1
        | color_type == c_PNG_COLOR_TYPE_GRAY_ALPHA = 2
        | color_type == c_PNG_COLOR_TYPE_RGB = 3
        | otherwise = 4
      alphaPos
        | color_type == c_PNG_COLOR_TYPE_GRAY || color_type == c_PNG_COLOR_TYPE_RGB = depth
        | otherwise = depth - 1
    
    let
        bytesInRow = fromIntegral$ depth*width
        nextForeign ptr = plusForeignPtr ptr bytesInRow
        next ptr = plusPtr ptr bytesInRow
    case outPtr of
        Nothing -> do
            rows <- if oneRow
                then sequence [rowEmpty depth$ width*height]
                else replicateM (fromIntegral $ height) (rowEmpty depth width)
            withForeignPtrs
                (if oneRow
                    then
                        take (fromIntegral height)$ iterate nextForeign$ foreignPtrRow$ head rows
                    else map foreignPtrRow rows) $
                \rowps -> withArray rowps $
                \rowpp -> c_png_read_image png rowpp
            c_png_read_end png nullPtr
            c_fclose file
            return$ Left$ PNGImage
                { widthImage = width
                , heightImage = height
                , depthImage = depth
                , alphaPosImage = alphaPos
                , rowsImage = rows }
        Just p -> do
            withArray (take (fromIntegral height)$ iterate next p)$ c_png_read_image png
            c_png_read_end png nullPtr
            c_fclose file
            return$ Right ()
    

-- --
-- writePNGFromPixelss :: String -> [[Pixel]] -> IO ()
-- writePNGFromPixelss pngFilename pixelss = do
--     image <- imageFromPixelss pixelss
--     writePNGImage pngFilename image

-- --
-- readPixelssFromPNG :: String -> IO [[Pixel]]
-- readPixelssFromPNG pngFilename = do
--     image <- readPNGImage False pngFilename
--     pixelssFromImage image
