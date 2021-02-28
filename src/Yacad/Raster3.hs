{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Yacad.Raster3 where

import Yacad.Raster.Expr

import Debug.Trace
import Text.Printf
import Data.List (intercalate)

import Control.Arrow
import qualified Data.Array as A
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


data Raster3 = Raster3
  { resolution :: ℝ3
  , raster :: A.Array (Int, Int, Int) Bool
  }
  deriving Show

-- $($(derive [d|
--     instance Store => Deriving (Store (Raster3 a))
--     |]))

box :: Raster3 -> (ℝ3, ℝ3)
box (Raster3 (xr, yr, zr) (A.bounds -> ((x1, y1, z1), (x2, y2, z2)))) =
  ( (xr * fromIntegral x1, yr * fromIntegral y1, zr * fromIntegral z1)
  , (xr * fromIntegral (x2+1), yr * fromIntegral (y2+1), zr * fromIntegral (z2+1))
  )

implicit_fn :: Raster3 -> ℝ3 -> ℝ
implicit_fn r = \p ->
  if r!p
    then -1
    else 1

implicit :: Raster3 -> SymbolicObj3
implicit raster = Cad.implicit (implicit_fn raster) (box raster)

blank :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> Raster3
blank dil res box =
  trace (show bnds)$ Raster3{resolution = res, raster = A.listArray bnds$ repeat False}
  where bnds = bounds dil res box

full :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> Raster3
full dil res box =
  Raster3{resolution = res, raster = A.listArray bnds$ repeat True}
  where bnds = bounds dil res box

fromImplicit :: ℝ -> ℝ3 -> (ℝ3, ℝ3) -> Obj3 -> Raster3
fromImplicit dil res box obj = 
  Raster3{resolution = res, raster = A.listArray bnds$ map (\pos -> obj pos <= dil) $ boxPoints res bnds}
  where
    bnds = bounds dil res box
      
bounds ::  ℝ -> ℝ3 -> (ℝ3, ℝ3) -> ((Int, Int, Int), (Int, Int, Int))
bounds dil res (start, end) = mapTuple (raster_ix res) (start + res/2 - (dil, dil, dil), end - res/2 + (dil, dil, dil))

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

boxPoints :: ℝ3 -> ((Int, Int, Int), (Int, Int, Int)) -> [ℝ3]
boxPoints res ((xi1, yi1, zi1), (xi2, yi2, zi2)) = [toWorld res (x, y, z) | x <- [xi1..xi2], y <- [yi1..yi2], z <- [zi1..zi2]]

rasterize :: ℝ -> ℝ3 -> SymbolicObj3 -> Raster3
rasterize dil res obj = fromImplicit dil res (getBox3 obj) (getImplicit3 obj)

(!) :: Raster3 -> ℝ3 -> Bool
(!) (Raster3 res raster) = \p ->
  let
    ix = raster_ix res p
  in
  inRange bounds ix && raster A.! ix
  where bounds = A.bounds raster

(//) :: Raster3 -> [(ℝ3, Bool)] -> Raster3
(//) old@(Raster3 res raster) xs = old
  {raster = raster A.// filter_valid (map (first$ raster_ix res) xs)}
  where filter_valid = filter$ (inRange$ A.bounds raster) . fst

(//@) :: Raster3 -> [((Int, Int, Int), Bool)] -> Raster3
(//@) old@(Raster3 _ raster) xs = old{raster = raster A.// filter_valid xs}
  where filter_valid = filter$ (inRange$ A.bounds raster) . fst

adjust :: ℝ3 -> ℝ3 -> ℝ3
adjust (xr, yr, zr) = \(x, y, z) -> (x / xr, y / yr, z/zr)

raster_ix :: ℝ3 -> ℝ3 -> (Int, Int, Int)
raster_ix (xr, yr, zr) = \(x, y, z) -> (floor$ x / xr, floor$ y / yr, floor$ z / zr)

toWorld :: ℝ3 -> (Int, Int, Int) -> ℝ3
toWorld (xr, yr, zr) = \(x,y,z) -> ((fromIntegral x+0.5)*xr, (fromIntegral y+0.5)*yr, (fromIntegral z+0.5)*zr)

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

floodFill :: ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> [ℝ3] -> (ℝ3 -> ℝ) -> ST s ()
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

floodShell :: ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> ℝ2 -> [ℝ3] -> (ℝ3 -> ℝ) -> ST s ()
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

floodFillE :: [ℝ3] -> (ℝ3 -> ℝ) -> Expr (ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> ST s ())
floodFillE frontier0 obj = Obj [(\res dil hasEffect write -> floodFill res dil hasEffect write frontier0 obj)]

floodShellE :: ℝ2 -> [ℝ3] -> (ℝ3 -> ℝ) -> Expr (ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> ST s ())
floodShellE shellRange frontier0 obj = Obj [(\res dil hasEffect write -> floodShell res dil hasEffect write shellRange frontier0 obj)]

toST :: Expr (ℝ3 -> ℝ -> [ℝ3]) -> Expr (ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> ST s ())
toST (Obj fns) = Obj$ map (\fn -> (\res dil _ write -> mapM_ write $ fn res dil)) fns
toST (Union exprs) = Union$ map toST exprs
toST (Diff exprs) = Diff$ map toST exprs

newtype FloodFill = FloodFill
  (forall s. Expr (ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> ST s ()))

modifyST :: Raster3 -> ℝ -> FloodFill -> Raster3
modifyST old@(Raster3 res d) dil expr = Raster3 res$ runSTArray action
  where
  action :: forall s. ST s (STArray s (Int, Int, Int) Bool)
  action = do
    md <- thaw d

    let FloodFill (expr' :: Expr (ℝ3 -> ℝ -> (ℝ3 -> ST s Bool) -> (ℝ3 -> ST s ()) -> ST s ())) = expr

    sequence$ do
      ((f, add), i) <- zip (run expr') [1..]
      let
        hasEffect :: ℝ3 -> ST s Bool
        hasEffect xyz
          | inRange (A.bounds d) coords =
            do
              v <- readArray md coords
              return$ v /= add
          | otherwise = return False
          where
            coords = raster_ix res xyz

        write :: ℝ3 -> ST s ()
        write xyz = writeArray md coords add
          where
            coords = raster_ix res xyz

      return$ trace (show i)$ f res (if add then dil else -dil) hasEffect write

    return md

  -- old // do
  -- ((f, add), i) <- zip (run expr) [1..]
  -- p <- trace (show i)$ f res$ if add then dil else -dil
  -- return (p, add)


fillBox :: ℝ3 -> ℝ -> Box3 -> Obj3 -> [ℝ3]
fillBox res dil box obj = filter (\pos -> obj pos <= dil)$ boxPoints res$ bounds dil res box

fillObj :: ℝ3 -> ℝ -> SymbolicObj3 -> [ℝ3]
fillObj res dil obj = fillBox res dil (getBox3 obj) (getImplicit3 obj)

fillCube :: ℝ3 -> ℝ -> Box3 -> [ℝ3]
fillCube res dil box = boxPoints res$ bounds dil res box

fillRast :: Raster3 -> [ℝ3]
fillRast (Raster3 res raster@(A.bounds -> ((x1, y1, z1), (x2, y2, z2)))) = 
  map (toWorld res)$ filter occupied $ points
  where
    occupied = (\pos -> raster A.! pos)
    points = [(x, y, z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

result :: (b -> b') -> ((a -> b) -> (a -> b'))
result =  (.)
swap_1_2 :: (a -> b -> c) -> b -> a -> c
swap_1_2 = flip    
swap_2_3 :: (a1 -> a2 -> b -> c) -> a1 -> b -> a2 -> c
swap_2_3 = result flip
swap_3_4 :: (a1 -> a2 -> a3 -> b -> c) -> a1 -> a2 -> b -> a3 -> c
swap_3_4 = (result.result) flip
swap_4_5 :: (a1 -> a2 -> a3 -> a4 -> b -> c) -> a1 -> a2 -> a3 -> b -> a4 -> c
swap_4_5 = (result.result.result) flip
swap_5_6 = (result.result.result.result) flip
shl_3 :: (a2 -> a1 -> b -> c) -> a1 -> b -> a2 -> c
shl_3 = swap_2_3.swap_1_2
shl_4 :: (a3 -> a1 -> a2 -> b -> c) -> a1 -> a2 -> b -> a3 -> c
shl_4 = swap_3_4.swap_2_3.swap_1_2
shl_5 = swap_4_5.swap_3_4.swap_2_3.swap_1_2
shl_6 = swap_5_6.swap_4_5.swap_3_4.swap_2_3.swap_1_2

fillObjE :: SymbolicObj3 -> Expr (ℝ3 -> ℝ -> [ℝ3])
fillObjE obj = Obj [(shl_3$ shl_3 fillObj) obj]

fillBoxE :: Box3 -> Obj3 -> Expr (ℝ3 -> ℝ -> [ℝ3])
fillBoxE box obj = Obj [(shl_4$ shl_4 fillBox) box obj]

fillCubeE :: Box3 -> Expr (ℝ3 -> ℝ -> [ℝ3])
fillCubeE box = Obj [(shl_3$ shl_3 fillCube) box]

fillRastE :: Raster3 -> Expr (ℝ3 -> ℝ -> [ℝ3])
fillRastE rast = Obj [(\_ _ -> fillRast rast)]

fillE :: [ℝ3] -> (ℝ3 -> ℝ) -> Expr (ℝ3 -> ℝ -> [ℝ3])
fillE frontier0 fn = Obj [(shl_4$ shl_4 fill) frontier0 fn]

translateE :: ℝ3 -> (ℝ3 -> ℝ3)
translateE v = (+v)

rotateE :: ℝ3 -> (ℝ3 -> ℝ3)
rotateE (yz, zx, xy) =
  let
        rotateYZ :: ℝ -> ℝ3 -> ℝ3
        rotateYZ θ (x,y,z) = ( x, y*cos θ - z*sin θ, z*cos θ + y*sin θ)
        rotateZX :: ℝ -> ℝ3 -> ℝ3
        rotateZX θ (x,y,z) = ( x*cos θ + z*sin θ, y, z*cos θ - x*sin θ)
        rotateXY :: ℝ -> ℝ3 -> ℝ3
        rotateXY θ (x,y,z) = ( x*cos θ - y*sin θ, y*cos θ + x*sin θ, z)
    in
        rotateXY xy . rotateZX zx . rotateYZ yz

scaleE :: ℝ3 -> (ℝ3 -> ℝ3)
scaleE v = (*v)

infixr 0 <~
(<~) :: (ℝ3 -> ℝ3) -> Expr (ℝ3 -> ℝ -> [ℝ3]) -> Expr (ℝ3 -> ℝ -> [ℝ3])
(<~) f (Obj fns) = Obj$ map (\fn -> (\res dil -> map f $ fn res dil)) fns
(<~) f (Union exprs) = Union$ map (f<~) exprs
(<~) f (Diff exprs) = Diff$ map (f<~) exprs

infixr 0 </~
(</~) :: (ℝ3 -> Bool) -> Expr (ℝ3 -> ℝ -> [ℝ3]) -> Expr (ℝ3 -> ℝ -> [ℝ3])
(</~) f (Obj fns) = Obj$ map (\fn -> (\res dil -> filter f $ fn res dil)) fns
(</~) f (Union exprs) = Union$ map (f</~) exprs
(</~) f (Diff exprs) = Diff$ map (f</~) exprs

modify :: Raster3 -> ℝ -> Expr (ℝ3 -> ℝ -> [ℝ3]) -> Raster3
modify old@(Raster3 res _) dil expr = old // do
  ((f, add), i) <- zip (run expr) [1..]
  p <- trace (show i)$ f res$ if add then dil else -dil
  return (p, add)

window :: (ℝ3 -> Bool -> [ℝ3]) -> Raster3 -> Raster3
window pixel_to_pixels (Raster3 res@(xr, yr, zr) arr) = Raster3 res$
  A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)
  where
    bnds = A.bounds arr
    pos_to_pixels p@(x, y, z) = filter (inRange bnds . fst)
      $ map (\(x, y, z) -> ((floor (x/xr), floor (y/yr), floor (z/zr)), True))
      $ pixel_to_pixels p' (arr A.! p)
      where p' = (xr * fromIntegral x, yr * fromIntegral y, zr * fromIntegral z)

window_int :: ((Int, Int, Int) -> Bool -> [(Int, Int, Int)]) -> Raster3 -> Raster3
window_int pixel_to_pixels rast@(Raster3 _ arr) = rast
  {raster = A.listArray bnds (repeat False) A.// concatMap pos_to_pixels (range bnds)}
  where
    bnds = A.bounds arr
    pos_to_pixels p = map (,True)$ filter (inRange bnds)$ pixel_to_pixels p (arr A.! p)

apply_mask :: [ℝ3] -> Raster3 -> Raster3
apply_mask mask rast@(Raster3 res@(xr, yr, zr) _) = window_int pixel_to_pixels rast
  where
    mask_int = map (\(x, y, z) -> (floor (x/xr), floor (y/yr), floor (z/zr))) mask
    pixel_to_pixels _ False = []
    pixel_to_pixels (px, py, pz) _ = map (\(x, y, z) -> (x + px, y + py, z + pz)) mask_int

dilate :: ℝ -> Raster3 -> Raster3
dilate r rast@(Raster3 res _) = apply_mask mask rast
  where mask = fill res 0 [(0, 0, 0)] (\(x, y, z) -> x^2 + y^2 +z^2 - r^2)

surrounding :: (Int, Int, Int) -> [(Int, Int, Int)]
surrounding = \coords -> map (+coords) d
  where
    ds = [-1, 0, 1]
    d = [(dx, dy, dz) | dx <- ds, dy <- ds, dz <- ds, dx /= 0 || dy /= 0 || dz /= 0]

example_shell = blank 0 (0.1, 0.1, 0.1) ((-1.3, -1.3, -1.3), (1.3, 1.3, 1.3)) //
  map (, True) (shell (0.05, 0.05, 0.05) [(1, 0, 0)] (\(x, y, z) -> x^2 + y^2 + z^2 - 1))

example_fill = blank 0 (0.1, 0.1, 0.1) ((-1.3, -1.3, -1.3), (1.3, 1.3, 1.3)) //
  map (, True) (fill (0.05, 0.05, 0.05) 0 [(0, 0, 0)] (\(x, y, z) -> x^2 + y^2 + z^2 - 1))

example_dilate = dilate 0.2 example_shell

showArr :: A.Array (Int, Int) Bool -> String
showArr arr = intercalate "\n" $
  flip map [y1..y2]$ \y ->
  flip map [x1..x2]$ \x -> if arr A.! (y, x) then '#' else ' '
  where ((y1, x1), (y2, x2)) = A.bounds arr
