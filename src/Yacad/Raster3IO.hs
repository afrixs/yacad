{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE FlexibleInstances #-}
module Yacad.Raster3IO where

import Yacad.Raster.Expr

import Debug.Trace
import Text.Printf
import Data.List (intercalate)

import Control.Arrow
import Data.Ix

import Data.List.Ordered
import qualified Graphics.Implicit as Cad
import Graphics.Implicit.Definitions
import Graphics.Implicit.ObjectUtil (getBox3, getImplicit3)

import Data.Array.ST
import Control.Monad.ST
import Control.Monad

-- import TH.Derive
-- import Data.Store

import Data.Array.Unsafe (unsafeForeignPtrToStorableArray, unsafeFreeze)
import Foreign (ForeignPtr, Word8, Storable)
import Control.DeepSeq
import System.CPUTime (getCPUTime)
import Data.Array.Storable as A

type Index = (Int, Int, Int)
type IndexBox = (Index, Index)

data Raster3 = Raster3
  { resolution :: ℝ3
  , rasterBounds :: IndexBox
  , raster :: A.StorableArray (Int, Int, Int) Word8
  }

-- $($(derive [d|
--     instance Store => Deriving (Store (Raster3 a))
--     |]))

box :: Raster3 -> (ℝ3, ℝ3)
box (Raster3 (xr, yr, zr) ((z1, y1, x1), (z2, y2, x2)) _) =
  ( (xr * fromIntegral x1, yr * fromIntegral y1, zr * fromIntegral z1)
  , (xr * fromIntegral (x2+1), yr * fromIntegral (y2+1), zr * fromIntegral (z2+1))
  )

implicit_fn :: Raster3 -> ℝ3 -> IO ℝ
implicit_fn r = \p -> do
  val <- r!p
  return$ if val
    then -1
    else 1

-- implicit :: Raster3 -> IO SymbolicObj3
-- implicit raster =
--   Cad.implicit (implicit_fn raster) (box raster)

blank :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> IO Raster3
blank dil res box =
  fromData dil res box$ repeat 0

full :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> IO Raster3
full dil res box =
  fromData dil res box$ repeat 255

fromImplicit :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> Obj3 -> IO Raster3
fromImplicit dil res box obj =
  fromData dil res box$ map (\pos -> if obj pos <= dil then 255 else 0) $ boxPoints res bnds
  where bnds = bounds dil res box

fromData :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> [Word8] -> IO Raster3
fromData dil res box d = do
  r <- A.newListArray bnds$ d
  return$ Raster3{resolution = res, rasterBounds = bnds, raster = r}
  where bnds = bounds dil res box

fromDataPtr :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> ForeignPtr Word8 -> IO Raster3
fromDataPtr dil res box d =
  do
    arr <- unsafeForeignPtrToStorableArray d bnds
    return Raster3{resolution = res, rasterBounds = bnds, raster = arr}
  where
    bnds = bounds dil res box

bounds ::  ℝ -> ℝ3 -> (ℝ3, ℝ3) -> IndexBox
bounds dil res (start, end) =
  mapTuple (raster_ix res) (start + res/2 - (dil, dil, dil), end - res/2 + (dil, dil, dil))

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)

callTriple :: (t1 -> a, t2 -> b, t3 -> c) -> (t1, t2, t3) -> (a, b, c)
callTriple (f1, f2, f3) (a1, a2, a3) = (f1 a1, f2 a2, f3 a3)

boxPoints :: ℝ3 -> IndexBox -> [ℝ3]
boxPoints res ((zi1, yi1, xi1), (zi2, yi2, xi2)) = [toWorld res (z, y, x) | z <- [zi1..zi2], y <- [yi1..yi2], x <- [xi1..xi2]]

rasterize :: ℝ -> ℝ3 -> SymbolicObj3 -> IO Raster3
rasterize dil res obj = fromImplicit dil res (getBox3 obj) (getImplicit3 obj)

(!) :: Raster3 -> ℝ3 -> IO Bool
(!) (Raster3 res bnds raster) = \p ->
  let
    ix = raster_ix res p
  in
  if inRange bnds ix
    then do 
      val <- A.readArray raster ix
      return$ val /= 0
    else return False

-- adjust :: ℝ3 -> ℝ3 -> ℝ3
-- adjust (xr, yr, zr) = \(x, y, z) -> (x / xr, y / yr, z/zr)

raster_ix :: ℝ3 -> ℝ3 -> Index
raster_ix (xr, yr, zr) = \(x, y, z) -> (floor$ z / zr, floor$ y / yr, floor$ x / xr)

toWorld :: ℝ3 -> Index -> ℝ3
toWorld (xr, yr, zr) = \(z, y, x) -> ((fromIntegral x+0.5)*xr, (fromIntegral y+0.5)*yr, (fromIntegral z+0.5)*zr)

shell :: ℝ3 -> [ℝ3] -> (ℝ3 -> ℝ) -> [ℝ3]
shell res@(xr, yr, zr) frontier0 fn =
  map (\(x, y, z) -> (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z))
  $ concat$ takeWhile (not . null)$ map snd stages
  where
    frontier = sort$ map (raster_ix res) frontier0
    stages = flip iterate ([], frontier)$ \(old, current) ->
      let
        seen = merge old current
        new = nub$ sort$ concatMap surrounding current
        new' = filter isBorder$ minus new seen
      in
        (current, new')

    isBorder (x, y, z) = (fn first_corner > 0) `elem` map (\p -> fn p <= 0) other_corners
      where
        (x', y', z') = (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z)
        first_corner = (x' - xrh, y' - yrh, z' - zrh)
        other_corners = drop 1 [(sx x' xrh, sy y' yrh, sz z' zrh) | sx <- signs, sy <- signs, sz <- signs]
          where
            signs = [(-), (+)]

    xrh = xr/2
    yrh = yr/2
    zrh = zr/2

fill :: ℝ3 -> ℝ -> [ℝ3] -> (ℝ3 -> ℝ) -> [ℝ3]
fill res dil frontier0 fn =
  map (toWorld res)$ concat$ takeWhile (not . null)$ map snd stages
  where
    frontier = sort$ map (raster_ix res) frontier0
    stages = flip iterate ([], frontier)$ \(old, current) ->
      let
        seen = merge old current
        new = nubSort$ concatMap surrounding current
        new' = filter isInside$ minus new seen
      in
        (current, new')

    isInside coords = fn (toWorld res coords) <= dil

floodFill :: ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> [ℝ3] -> (ℝ3 -> ℝ) -> IO ()
floodFill res@(rx, ry, rz) dil hasEffect write frontier0 fn
  = mapM_ go$ map fixp frontier0
      where
        fixp p = toWorld res$ raster_ix res p
        go p@(x, y, z) = do
          he <- hasEffect p
          when (he && (fn p <= dil)) $ do
            write p
            mapM_ go $ surrounding
          where
            surrounding =
              [ (x-rx, y, z), (x, y-ry, z), (x, y, z-rz)
              , (x+rx, y, z), (x, y+ry, z), (x, y, z+rz)
              ]

floodShell :: ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> ℝ2 -> [ℝ3] -> (ℝ3 -> ℝ) -> IO ()
floodShell res@(rx, ry, rz) dil hasEffect write (shellStart, shellEnd) frontier0 fn =
  stage ([], frontier)
  where
    frontier = sort$ map (raster_ix res) frontier0
    stage (_, []) =
      return ()
    stage (old, current) =
      let
        surrounding (x, y, z) =
              [ (x-1, y, z), (x, y-1, z), (x, y, z-1)
              , (x+1, y, z), (x, y+1, z), (x, y, z+1)
              ]
        seen = merge old current
        new = nubSort$ concatMap surrounding current
      in
      do
        new' <- filterM process$ minus new seen
        stage (current, new')
    
    process coords = do
      he <- hasEffect p
      let isInside = he && (val <= shellEnd + dil)
      when (isInside && (val >= shellStart - dil))$ write p
      return$ isInside
      where
        p = toWorld res coords
        val = fn p

fillRast :: Box3 -> Raster3 -> (ℝ3 -> ℝ3) -> Bool -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ()
fillRast (fstart, fend) (Raster3 res (rstart, rend) d) trans inverted dil hasEffect write =
  mapM_ writeIfOccupied points
  where
    writeIfOccupied :: Index -> IO ()
    writeIfOccupied index =
      do
        occ <- occupied index
        when occ$ do
          let pt = toWld index
          he <- hasEffect pt
          when he$ write pt
    occupied :: Index -> IO Bool
    occupied
      | inverted = \index -> do
          occ <- readArray d index
          return$ occ == 0
      | otherwise = \index -> do
          occ <- readArray d index
          return$ occ /= 0
    toWld = trans . toWorld res
    s@(z1, y1, x1) = callTriple (mapTriple max rstart)$ raster_ix res (fstart + res/2 - (dil, dil, dil))
    e@(z2, y2, x2) = callTriple (mapTriple min rend)$ raster_ix res (fend - res/2 + (dil, dil, dil))
    points = [(z, y, x) | z <- [z1..z2], y <- [y1..y2], x <- [x1..x2]]

floodFillE :: [ℝ3] -> (ℝ3 -> ℝ) -> Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ())
floodFillE frontier0 obj = Obj [(\res dil hasEffect write -> floodFill res dil hasEffect write frontier0 obj)]

floodShellE :: ℝ2 -> [ℝ3] -> (ℝ3 -> ℝ) -> Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ())
floodShellE shellRange frontier0 obj = Obj [(\res dil hasEffect write -> floodShell res dil hasEffect write shellRange frontier0 obj)]

fillRastE :: Raster3 -> (ℝ3 -> ℝ3) -> Bool -> Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ())
fillRastE raster = fillRastBoxE (mapTuple (toWorld$ resolution raster)$ rasterBounds raster) raster

fillRastBoxE :: Box3 -> Raster3 -> (ℝ3 -> ℝ3) -> Bool -> Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ())
fillRastBoxE srcbox raster trans inverted = Obj [(\_ dil hasEffect write -> fillRast srcbox raster trans inverted dil hasEffect write)]

toIO :: Expr (ℝ3 -> ℝ -> [ℝ3]) -> Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ())
toIO (Obj fns) =
  Obj$ map
    (\fn res dil hasEffect write ->
        mapM_
          (\pt -> do
            he <- hasEffect pt
            when he$ write pt
          )$
          fn res dil
    )
    fns

toIO (Union exprs) = Union$ map toIO exprs
toIO (Diff exprs) = Diff$ map toIO exprs

newtype FloodFill = FloodFill
  (forall s. Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ()))

modifyIO :: Raster3 -> ℝ -> FloodFill -> IO ()
modifyIO old@(Raster3 res bnds@((x0, y0, z0), (x1, y1, z1)) d) dil expr = action
  where
  action :: IO ()
  action = do
    let
      scaleViz :: ℝ
      scaleViz = minimum [30.0/fromIntegral (x1 - x0), 30.0/fromIntegral (y1 - y0), 30.0/fromIntegral (z1 - z0)]
      bndsViz :: ((Int, Int), (Int, Int))
      bndsViz =
        ( ( floor$ fromIntegral x0*scaleViz + fromIntegral z0/2*scaleViz
          , floor$ fromIntegral y0*scaleViz + fromIntegral z0/2*scaleViz
          )
        , ( floor$ fromIntegral x1*scaleViz + fromIntegral z1/2*scaleViz
          , floor$ fromIntegral y1*scaleViz + fromIntegral z1/2*scaleViz
          )
        )
      
      FloodFill (expr' :: Expr (ℝ3 -> ℝ -> (ℝ3 -> IO Bool) -> (ℝ3 -> IO ()) -> IO ())) = expr
    
    sequence$ do
      ((f, add), i) <- zip (run expr') [1..]
      let val = if add then 255 else 0
      let
        fromWld = raster_ix res
        hasEffect :: ℝ3 -> IO Bool
        hasEffect xyz
          | inRange bnds coords =
            do
              v <- readArray d coords
              return$ v /= val
          | otherwise = return False
          where
            coords = fromWld xyz

        write :: ℝ3 -> IO ()
        write xyz = writeArray d (fromWld xyz) val

      return$ trace (show i)$ f res (if add then dil else -dil) hasEffect write

    return ()

  -- old // do
  -- ((f, add), i) <- zip (run expr) [1..]
  -- p <- trace (show i)$ f res$ if add then dil else -dil
  -- return (p, add)

-- modify :: Raster3 -> ℝ -> Expr (ℝ3 -> ℝ -> [ℝ3]) -> Raster3
-- modify old@(Raster3 res _ _) dil expr = old // do
--   ((f, add), i) <- zip (run expr) [1..]
--   p <- trace (show i)$ f res$ if add then dil else -dil
--   return (p, if add then 255 else 0)

-- window :: (ℝ3 -> Bool -> [ℝ3]) -> Raster3 -> Raster3
-- window pixel_to_pixels (Raster3 res@(xr, yr, zr) arr) = Raster3 res$
--   A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)
--   where
--     bnds = A.bounds arr
--     pos_to_pixels p@(x, y, z) = filter (inRange bnds . fst)
--       $ map (\(x, y, z) -> ((floor (x/xr), floor (y/yr), floor (z/zr)), True))
--       $ pixel_to_pixels p' (arr A.! p)
--       where p' = (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z)

-- window_int :: ((Int, Int, Int) -> Bool -> [(Int, Int, Int)]) -> Raster3 -> Raster3
-- window_int pixel_to_pixels rast@(Raster3 _ arr) = rast
--   {raster = A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)}
--   where
--     bnds = A.bounds arr
--     pos_to_pixels p = map (,True)$ filter (inRange bnds)$ pixel_to_pixels p (arr A.! p)

-- apply_mask :: [ℝ3] -> Raster3 -> Raster3
-- apply_mask mask rast@(Raster3 res@(xr, yr, zr) _) = window_int pixel_to_pixels rast
--   where
--     mask_int = map (\(x, y, z) -> (floor (x/xr), floor (y/yr), floor (z/zr))) mask
--     pixel_to_pixels _ False = []
--     pixel_to_pixels (px, py, pz) _ = map (\(x, y, z) -> (x + px, y + py, z + pz)) mask_int

-- dilate :: ℝ -> Raster3 -> Raster3
-- dilate r rast@(Raster3 res _) = apply_mask mask rast
--   where mask = fill res 0 [(0, 0, 0)] (\(x, y, z) -> x^2 + y^2 +z^2 - r^2)

surrounding :: (Int, Int, Int) -> [(Int, Int, Int)]
surrounding = \coords -> map (+coords) d
  where
    ds = [-1, 0, 1]
    d = [(dx, dy, dz) | dx <- ds, dy <- ds, dz <- ds, dx /= 0 || dy /= 0 || dz /= 0]

-- example_shell = blank 0 (0.1, 0.1, 0.1) ((-1.3, -1.3, -1.3), (1.3, 1.3, 1.3)) //
--   map (, True) (shell (0.05, 0.05, 0.05) [(1, 0, 0)] (\(x, y, z) -> x^2 + y^2 + z^2 - 1))

-- example_fill = blank 0 (0.1, 0.1, 0.1) ((-1.3, -1.3, -1.3), (1.3, 1.3, 1.3)) //
--   map (, True) (fill (0.05, 0.05, 0.05) 0 [(0, 0, 0)] (\(x, y, z) -> x^2 + y^2 + z^2 - 1))

-- example_dilate = dilate 0.2 example_shell

-- showArr :: A.S (Int, Int) Bool -> String
-- showArr arr = intercalate "\n" $
--   flip map [y1..y2]$ \y ->
--   flip map [x1..x2]$ \x -> if arr A.! (y, x) then '#' else ' '
--   where ((y1, x1), (y2, x2)) = A.bounds arr
