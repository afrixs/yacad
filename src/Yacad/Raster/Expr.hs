{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

module Yacad.Raster.Expr where

import Data.Functor.Foldable
-- import Data.Fix

-- FIXME: Diff deletes points from upper layers
--DeriveTraversable, deriving (Functor, Foldable, Traversable)
--(traverse = fmap, ale mozes pouzit aplikativnu funkciu (tym padom monadicku))


-- Boilerplate begins here

data ExprF point rec
  = UnionF [rec]
  | DiffF [rec]
  | ObjF [point]
  deriving (Functor, Show)

type Expr point = Fix (ExprF point)

pattern Union xs = Fix (UnionF xs)
pattern Diff xs = Fix (DiffF xs)
pattern Obj pts = Fix (ObjF pts)

-- Boilerplate end
-------------------------------------------------------------------
-- Real logic begins here

run :: Expr point -> [(point, Bool)]
run = elgot concat coalg . (True,)

coalg :: (Bool, Expr point) -> Either [(point, Bool)] [(Bool, Expr point)]
coalg (add, Union xs)    = Right$ map (add,) xs
coalg (add, Diff (x:xs)) = Right$ (add, x) : map (not add,) xs
coalg (add, Obj pts)     = Left $ map (, add) pts

------------------------------------------------------------------
-- Explicit implementation

data ExprE point
  = UnionE [ExprE point]
  | DiffE [ExprE point]
  | ObjE [point]
  deriving Show

runE :: ExprE point -> [(point, Bool)]
runE = recE . (True,)

recE :: (Bool, ExprE point) -> [(point, Bool)]
recE (add, UnionE xs)    = concatMap (recE . (add,)) xs
recE (add, DiffE (x:xs)) = recE (add, x) ++ concatMap (recE . (not add,)) xs
recE (add, ObjE pts)     = map (, add) pts
