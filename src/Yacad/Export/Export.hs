{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Yacad.Export.Export where

import Yacad.Raster3 as Ra3

import qualified Yacad.Raster3IO as Ra3IO

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.ByteString.Lazy as BL
import Data.Store ( encode )
import qualified Data.ByteString.Builder as BB
import qualified Codec.Picture as Pic
import qualified Text.Printf as PF
import Text.XML.HXT.Core
import Text.Scanf
import System.Directory
    ( doesFileExist,
      getDirectoryContents,
      createDirectory,
      removeDirectoryRecursive,
      doesDirectoryExist )
import Control.Monad
import Debug.Trace as Debug
import Control.DeepSeq
import System.CPUTime

import Codec.Image.LibPNG
import Data.List (sort, unfoldr)
import Foreign (withArray, nullPtr, peekArray, withForeignPtr, Word8, plusForeignPtr, peek, ForeignPtr, Ptr, castPtr, castForeignPtr)
import Foreign.C (withCString)
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Graphics.Implicit.Definitions (ℝ3, Box3)
import Data.Array.Storable
import qualified Data.Vector.Storable as V

writeRa3 :: String -> Raster3 -> IO ()
writeRa3 name (Raster3 _ ra) = 
  let
    ((x1, y1, z1), (x2, y2, z2)) = A.bounds ra
    (sx, sy, sz) = (x2-x1, y2-y1, z2-z1)
  in
  BL.writeFile name $ BB.toLazyByteString
      $ BB.int32BE (fromIntegral$ sx) <> BB.int32BE (fromIntegral$ sy) <> BB.int32BE (fromIntegral$ sz)
      <> (BB.byteString$ encode ra)

writeSVXIO :: Bool -> String -> Ra3IO.Raster3 -> IO ()
writeSVXIO brokenSlicesOr name raster@(Ra3IO.Raster3 (rx, _, _) ((z1, y1, x1), (z2, y2, x2)) ra) =
    do
      let
        size@(sx, sy, sz) = (x2-x1+1, y2-y1+1, z2-z1+1)
        boxf@((_, y1f, z1f), (_, y2f, z2f)) = mapTuple (fixSlicesOr brokenSlicesOr)$ ((x1, y1, z1), (x2, y2, z2))
        sizef@(sxf, syf, szf) = (fixSlicesOr brokenSlicesOr) size
      exists <- doesDirectoryExist name
      if exists then removeDirectoryRecursive name else return ()
      createDirectory name
      createDirectory$ name ++ "/density"
      mapM_ (\z -> do
                a <- mapM (\(x, y) -> readArray ra (z1 + z, y1 + y, x1 + x)) [(x, y) | y <- [0..sy-1], x <- [0..sx-1]]
                Pic.savePngImage (PF.printf "%s/density/slice%04d.png" name z)
                      $ Pic.ImageY8
                      $ Pic.Image sx sy$ V.fromList a --Pic.generateImage (\x y -> readArray ra (x1+x, y1+y, z1+z)) sx sy
            )
            [0..sz-1]
      BL.writeFile (name ++ "/manifest.xml")$ BB.toLazyByteString$ BB.stringUtf8
        $ PF.printf
            "<?xml version=\"1.0\"?>\n\
            \<grid version=\"1.0\" gridSizeX=\"%d\" gridSizeY=\"%d\" gridSizeZ=\"%d\" \n\
            \ originX=\"%f\" originY=\"%f\" originZ=\"%f\"\n\
            \ voxelSize=\"%f\" subvoxelBits=\"8\" slicesOrientation=\"Z\" >\n\
            \    <channels>\n\
            \        <channel type=\"DENSITY\" bits=\"8\" slices=\"density/slice%s.png\" />\n\
            \    </channels>\n\n\
            \    <materials>\n\
            \        <material id=\"1\" urn=\"urn:shapeways:materials/1\" />\n\
            \    </materials>\n\n\
            \    <metadata>\n\
            \        <entry key=\"author\" value=\"Yacad\" />\n\
            \        <entry key=\"creationDate\" value=\"2020/11/29\" />\n\
            \    </metadata>\n\
            \</grid>"
            (sx) (syf) (szf)
            (fromIntegral x1*rx*0.001) (fromIntegral y1f*rx*0.001) (fromIntegral z1f*rx*0.001)
            (rx/1000) "%04d"

writeSVX :: Bool -> String -> Raster3 -> IO ()
writeSVX brokenSlicesOr name (Raster3 (rx, _, _) ra) =
  let
    box@((x1, y1, z1), (x2, y2, z2)) = A.bounds ra
    size@(sx, sy, sz) = (x2-x1+1, y2-y1+1, z2-z1+1)
    boxf@((_, y1f, z1f), (_, y2f, z2f)) = mapTuple (fixSlicesOr brokenSlicesOr)$ box
    sizef@(sxf, syf, szf) = (fixSlicesOr brokenSlicesOr) size
  in
    do
      exists <- doesDirectoryExist name
      if exists then removeDirectoryRecursive name else return ()
      createDirectory name
      createDirectory$ name ++ "/density"
      mapM_ (\z -> Pic.savePngImage (PF.printf "%s/density/slice%04d.png" name z)
                  $ Pic.ImageY8$ Pic.generateImage (\x y -> ra A.! (x1+x, y1+y, z1+z)) sx sy) [0..sz-1]
      BL.writeFile (name ++ "/manifest.xml")$ BB.toLazyByteString$ BB.stringUtf8
        $ PF.printf
            "<?xml version=\"1.0\"?>\n\
            \<grid version=\"1.0\" gridSizeX=\"%d\" gridSizeY=\"%d\" gridSizeZ=\"%d\" \n\
            \ originX=\"%f\" originY=\"%f\" originZ=\"%f\"\n\
            \ voxelSize=\"%f\" subvoxelBits=\"8\" slicesOrientation=\"Z\" >\n\
            \    <channels>\n\
            \        <channel type=\"DENSITY\" bits=\"8\" slices=\"density/slice%s.png\" />\n\
            \    </channels>\n\n\
            \    <materials>\n\
            \        <material id=\"1\" urn=\"urn:shapeways:materials/1\" />\n\
            \    </materials>\n\n\
            \    <metadata>\n\
            \        <entry key=\"author\" value=\"Yacad\" />\n\
            \        <entry key=\"creationDate\" value=\"2020/11/29\" />\n\
            \    </metadata>\n\
            \</grid>"
            (sx) (syf) (szf)
            (fromIntegral x1*rx*0.001) (fromIntegral y1f*rx*0.001) (fromIntegral z1f*rx*0.001)
            (rx/1000) "%04d"

fixSlicesOr :: Bool -> (a, a, a) -> (a, a, a)
fixSlicesOr False = id
fixSlicesOr True = \(x, y, z) -> (x, z, y)

--TODO: use for export as well
--TODO: attrs from list (foldl)
-- manifestAttrs :: [String]
-- manifestAttrs = ["gridSizeX", "gridSizeY", "gridSizeZ", "originX", "originY", "originZ", "voxelSize"]
xpRaster :: Bool -> PU (ℝ3, Box3)
xpRaster brokenSlicesOr
    = xpElem "grid"$ 
      xpFilterAttr (
        hasName "gridSizeX" 
                <+> hasName "gridSizeY"
                <+> hasName "gridSizeZ"
                <+> hasName "originX"
                <+> hasName "originY"
                <+> hasName "originZ"
                <+> hasName "voxelSize"
                )$
      xpFilterCont (none)$
      xpWrap 
        ( newRaster
        , \((fixSlicesOr brokenSlicesOr) -> res@(rx, ry, rz), (s, e)) ->
              let ((x1, y1, z1), (x2, y2, z2)) = (raster_ix res s, raster_ix res e)
              in
              (x2-x1, y2-y1, z2-z1, fromIntegral x1*rx, fromIntegral y1*ry, fromIntegral z1*rz, rx*0.001)
        )$
      xp7Tuple (xpAttr "gridSizeX" xpPrim)
               (xpAttr "gridSizeY" xpPrim)
               (xpAttr "gridSizeZ" xpPrim)
               (xpAttr "originX"   xpPrim)
               (xpAttr "originY"   xpPrim)
               (xpAttr "originZ"   xpPrim)
               (xpAttr "voxelSize" xpPrim)
      where
        newRaster (sx, sy, sz, x, y, z, r) =
          -- let
          --   rast (Left ddd) = return$ fromData dil res box$ repeat ddd
          --   rast (Right ddd) = fromDataPtr dil res box ddd
          -- in
          -- rast d
          (res, box)
          where
            res = (rr, rr, rr)
            box = mapTuple (fixSlicesOr brokenSlicesOr)
                           ( (xx+rr*0.5, yy+rr*0.5, zz+rr*0.5)
                           , (xx+(fromIntegral sx-0.5)*rr
                           , yy+(fromIntegral sy-0.5)*rr
                           , zz+(fromIntegral sz-0.5)*rr)
                           )
            (xx, yy, zz, rr) = (x, y, z, r)*1000

readSVX :: Bool -> String -> IO Raster3
readSVX brokenSlicesOr name = 
  do
    [(res, box)] <- runX 
                  ( xunpickleDocument (xpRaster brokenSlicesOr)
                                [ withValidate no
                                , withTrace 0
                                , withRemoveWS yes
                                , withPreserveComment no
                                ]$ name ++ "/manifest.xml"
                  )
    files <- getDirectoryContents$ dirName
    let bnds = bounds 0.001 res box
    slices <- fillSlices bnds files
    return$ Raster3 {resolution = res, raster = (A.listArray bnds$ repeat 0) A.// (map (, 255)$ slices)}
  where
    dirName :: String
    dirName = name ++ "/density/"

    fillSlices :: ((Int, Int, Int), (Int, Int, Int)) -> [FilePath] -> IO [(Int, Int, Int)]
    fillSlices ((x1, y1, z1), (x2, y2, _)) files = do
        slices <- sequence$ map fillSlice files
        return$ concat slices
      where
        fillSlice :: FilePath -> IO [(Int, Int, Int)]
        fillSlice file@(scanf [fmt|slice%d.png|] -> parsed) = 
          case parsed of
            Nothing -> return []
            (Just (z :+ ())) -> do
              imgLoading <- Pic.readPng$ dirName ++ file
              case imgLoading of
                Left err -> return$ Debug.trace ("error loading " ++ file ++ ": " ++ err)$ []
                Right (Pic.ImageY8 img) ->
                  return$ map (\(x, y) -> (x1 + x, y1 + y, z1 + z))$
                          filter (\(x, y) -> Pic.pixelAt img x y /= 0)
                          [(x, y) | x <- [0..x2-x1], y <- [0..y2-y1]]
                Right _ -> return$ Debug.trace ("Unsupported image format " ++ file)$ []

readSVXFast :: Bool -> String -> IO Ra3IO.Raster3
readSVXFast brokenSlicesOr name = 
  do
    files <- getDirectoryContents$ dirName
    [(res, box)] <- runX 
                  ( xunpickleDocument (xpRaster brokenSlicesOr)
                                [ withValidate no
                                , withTrace 0
                                , withRemoveWS yes
                                , withPreserveComment no
                                ]$ name ++ "/manifest.xml"
                  )
    slices <- loadSlices files
    Ra3IO.fromDataPtr 0.001 res box slices
  where
    dirName :: String
    dirName = name ++ "/density/"

    loadSlices :: [FilePath] -> IO (ForeignPtr Word8)
    loadSlices files = do
        fls <- filterM (\f -> doesFileExist$ dirName ++ f)$ sort files
        (Left (PNGImage w h d apos _)) <- readPNGImage True Nothing$ dirName ++ head fls
        start <- getCPUTime
        dat <- rowEmpty d$ w*h*fromIntegral (length fls)
        let imgSize = fromIntegral$ w*h*d
        Debug.trace (show imgSize)$ withForeignPtr (foreignPtrRow dat)$ \ptr -> mapM fillSlice$ zip fls$ iterate (\p -> plusPtr p imgSize) ptr
        end <- dat `deepseq` getCPUTime
        return$ foreignPtrRow dat
      where
        fillSlice :: (FilePath, Ptr Word8) -> IO ()
        fillSlice (file, ptr) = 
          do
            let path = dirName ++ file
            (Right _) <- readPNGImage True (Just ptr) path
            return ()


readSVXFast0 :: Bool -> String -> IO Raster3
readSVXFast0 brokenSlicesOr name = 
  do
    [(res, box)] <- runX 
                  ( xunpickleDocument (xpRaster brokenSlicesOr)
                                [ withValidate no
                                , withTrace 0
                                , withRemoveWS yes
                                , withPreserveComment no
                                ]$ name ++ "/manifest.xml"
                  )
    files <- getDirectoryContents$ dirName
    let bnds = bounds 0.001 res box
    slices <- loadSlices files
    return$ fromData 0.001 res box slices
  where
    dirName :: String
    dirName = name ++ "/density/"

    loadSlices :: [FilePath] -> IO [Word8]
    loadSlices files = do
        slices <- sequence$ map fillSlice$ sort files
        return$ concat slices
      where
        fillSlice :: FilePath -> IO [Word8]
        fillSlice file = 
          do
            let path = dirName ++ file
            isFile <- doesFileExist$ path
            if isFile
              then do
                (Left img@(PNGImage w h d apos dat)) <- readPNGImage True Nothing path
                let
                  occsFromRow (PNGRow rowfp) = withForeignPtr rowfp occsFromRowPtr
                  depth = fromIntegral d

                  occsFromRowPtr :: Ptr Word8 -> IO [Word8]
                  occsFromRowPtr rowp =
                    let
                      next ptr = plusPtr ptr depth
                    in
                      -- peekArray (fromIntegral$ d*w*h) rowp
                    mapM peek$ take (fromIntegral$ w*h)$ iterate next rowp
                    -- do
                    --   ws <- peekArray (fromIntegral$ d*w*h) rowp -- w*h because image is oneRow
                    --   return$ unfoldr occFromList ws
                  
                  occFromList :: [Word8] -> Maybe ( Bool, [Word8] )
                  occFromList (w:dat) = Just (w /= 0, drop (fromIntegral d-1) dat)
                  occFromList _ = Nothing

                occs <- occsFromRow$ head dat
                return occs
              else
                return []

--TODO
-- https://getqubicle.com/qubicle/documentation/docs/file/qb/
-- B.writeFile "test.qb"
--   $ BB.toLazyByteString
--      $ BB.word8 1 <> BB.word8 1 <> BB.word8 0 <> BB.word8 0
--     <> BB.int32BE 0 <> BB.int32BE 1 <> BB.int32BE 0 <> BB.int32BE 0 <> BB.int32BE 1
--     <> BB.word8 3 <> BB.stringUtf8 "obj"
--     <> BB.int32BE (fromIntegral$ x2-x1) <> BB.int32BE (fromIntegral$ y2-y1) <> BB.int32BE (fromIntegral$ z2-z1)
--     <> BB.int32BE (fromIntegral$ x1) <> BB.int32BE (fromIntegral$ y1) <> BB.int32BE (fromIntegral$ z1)
--     <> --(BB.byteString$ runPut . mapM_ (\p -> putWord8$ if sm p <= 0 then 1 else 255)$ boxPoints res box)
