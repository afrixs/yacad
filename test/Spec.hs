-- import Data.Array.IO
-- import Control.Monad
-- import System.CPUTime
-- import Data.Time

-- main :: IO ()
-- main = do
--   startt <- getCurrentTime 
--   start <- getCPUTime
--   arr <- newArray (0, 100) 0 :: IO (IOUArray Int Int)
--   let
--     okSimple i =
--       i < 100

--     ok i = do
--       return$ i < 100
--     -- -- of course we don't need IO for a simple i < 100
--     -- -- but my goal is to ask for the contents of the array, ie.
--     -- ok i = do
--     --   current <- readArray arr (i `mod` 101)
--     --   return$ i `mod` 37 > current `mod` 37
    
--     write :: Int -> IO ()
--     write i =
--       writeArray arr (i `mod` 101) i

--     writeIfOkSimple :: Int -> IO ()
--     writeIfOkSimple i =
--       when (okSimple i)$ write i

--     writeIfOk :: Int -> IO ()
--     writeIfOk i =
--       ok i >>= (\isOk -> when isOk$ write i)

--   -------------------------------------------------------------------
--   ---- these four methods have approximately same execution time ----
--   ---- (but the last one is executed on 250 times shorter list)  ----
--   -------------------------------------------------------------------
--   -- mapM_ write$ filter okSimple [0..10000000*250] -- t = 20.694
--   -- mapM_ writeIfOkSimple [0..10000000*250]        -- t = 20.698
--   -- mapM_ writeIfOk [0..10000000*250]              -- t = 20.669
--   filterM ok [0..1000000000] >>= mapM_ write          -- t = 17.200

--   -- evaluate array
--   elems <- getElems arr
--   print$ sum elems

--   end <- getCPUTime
--   endt <- getCurrentTime
--   print$ fromIntegral (end - start) / (10^12)
--   print (diffUTCTime endt startt)
  
import Yacad.Raster3 as Ra3
import qualified Yacad.Raster3IO as Ra3IO
import Yacad.Raster.Expr
import Yacad.Export.Export
import Graphics.Implicit
import Debug.Trace
import Control.DeepSeq
import System.CPUTime
import Text.Printf
import Graphics.Implicit.Definitions (ℝ3)

instance NFData Raster3 where
  rnf r = rnf (Ra3.resolution r) `seq` rnf (Ra3.raster r)

-- instance NFData Ra3IO.Raster3 where
--   rnf r = rnf (Ra3IO.resolution r) `seq` rnf (Ra3IO.raster r)

main :: IO ()
main = 
  let
  --   chunkCnt = (sx`div`255)*(sy`div`255)*(sz`div`255)
  in
  do
  -- writeSVG 0.1 "test.svg"$ Ra2.implicit Ra2.example_shell
  -- writeSTL 0.1 "test-rasterize.stl"$ Ra3.implicit$ Ra3.rasterize 0.1
  --   $ union
  --     [ sphere 1.2
  --     , translate (0, 0, 2.1)$ sphere 1
  --     , translate (0, 0, 2.1)
  --         $ rotate3 (0, pi/2, 0) 
  --             $ extrudeRM 0 (Left 0) (Right (\z -> 1-z*0.5)) (Left (0, 0)) (circle 0.3) (Left 2)
  --     , translate (0, 0, 3.1)
  --         $ extrudeOnEdgeOf 
  --               (polygonR 0 [(0.3, 0), (0.3, 0.2), (0, 0.2), (0, 1), (-0.5, 1), (-0.5, 0)])
  --             $ circle(0.5)
  --     ]

    -- generation

    start <- trace "\nsnowman"$ getCPUTime
    end <- snowmanRa `deepseq` getCPUTime
    trace (printf "snowman: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    -- IO vs ST

    start <- trace "st snowman"$ getCPUTime
    let ra = snowmanST
    writeSVX True "testm-svx-st" ra
    end <- getCPUTime
    trace (printf "st snowman: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- trace "io snowman"$ getCPUTime
    ra <- snowmanIO
    writeSVXIO True "testm-svx-io" ra
    end <- getCPUTime
    trace (printf "io snowman: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- trace "\nsvx import-export"$ getCPUTime
    ra <- readSVX True "testm-svx-st"
    writeSVX True "testm-svx-from-svx" ra
    end <- getCPUTime
    trace (printf "svx import-export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- trace "svx io import-export"$ getCPUTime
    ra <- readSVXFast True "testm-svx-io"
    writeSVXIO True "testm-svx-io-from-svx" ra
    end <- getCPUTime
    trace (printf "svx io import-export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    -- IO fillRast

    start <- trace "io fillRast"$ getCPUTime
    ra <- readSVXFast True "testm-svx-io"
    let ra2bnds@(ra2start, ra2end) = ((-1.5, -1.22, 1.0), (2.0, 1.22, 2.2))
        ra2res = 0.02 :: ℝ3
    ra2 <- Ra3IO.blank 0 0.02 ra2bnds
    Ra3IO.modifyIO ra2 0.0001$ Ra3IO.FloodFill$ Ra3IO.fillRastBoxE (ra2start + ra2res, ra2end - ra2res) ra id False
    writeSVXIO True "testm-svx-io-fillrast" ra2
    end <- getCPUTime
    trace (printf "svx io import-export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    -- various exports and import

    start <- getCPUTime
    trace "ra3"$ writeRa3 "testm.ra3" snowmanST
    end <- getCPUTime
    trace (printf "ra3 export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- getCPUTime
    trace "svx"$ writeSVX True "testm-svx" snowmanST
    end <- getCPUTime
    trace (printf "svx export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- getCPUTime
    trace "stl"$ writeSTL 0.1 "testm-stl.stl"$ Ra3.implicit$ snowmanST
    end <- getCPUTime
    trace (printf "stl export: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- trace "svx import"$ getCPUTime
    ra <- readSVX True "testm-svx"
    end <- ra `deepseq` getCPUTime
    trace (printf "svx import: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    -- combine

    start <- trace "combine implicit"$ getCPUTime
    end <- (modify (Ra3.blank 0 0.02 ((-1.5, -1.2, -1.35), (2.0, 1.2, 4.2))) (-0.0001)$ Union [
        fillObjE$ Ra3.implicit ra
      , fillObjE$ Ra3.implicit snowmanST
      ]) `deepseq` getCPUTime
    trace (printf "combine rasters implicit: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    start <- trace "combine fillRast"$ getCPUTime
    end <- (modify (Ra3.blank (-0.0001) 0.02 ((-1.5, -1.2, -1.35), (2.0, 1.2, 4.2))) (-0.0001)$ Union [
        fillRastE$ ra
      , fillRastE$ snowmanST
      ]) `deepseq` getCPUTime
    trace (printf "combine rasters fillRast: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

    trace "svx-from-svx"$ writeSVX True "testm-svx-from-svx" ra
    trace "stl-from-svx"$ writeSTL 0.1 "testm-stl-from-svx.stl"$ Ra3.implicit$ ra

    -- dilatation

    let dil = -0.0001
    let cube = modify (Ra3.blank dil 0.5 ((-0.75, -0.5, -0.5), (2.1, 2.5, 2.5))) dil$ Diff [
            Ra3.fillCubeE ((-0.25, 0.0, -0.5), (1.5, 2.5, 2.25))
          , Ra3.fillCubeE ((0.25, 1, 1), (1.25, 1.75, 2.1))
          ]
    trace "cubeDilTest"$ writeSVX True "testm-cube-dil" cube
    ra <- readSVX True "testm-cube-dil"
    trace "cubeDilExportTest"$ writeSVX True "testm-cube-dil-from-svx" ra

    let cubeImplicit = modify (Ra3.blank dil 0.5 ((-0.75, -0.5, -0.5), (2.1, 2.5, 2.5))) dil$ Diff [
            fillObjE$ extrudeOnEdgeOf (rectR 0 (-5, -0.5) (0, 2.25)) (rectR 0 (-0.25, 0.0) (1.5, 2.5))
          , Ra3.fillCubeE ((0.25, 1, 1), (1.25, 1.75, 2.1))
          ]
    trace "cubeDilTestImplicit"$ writeSVX True "testm-cube-dil-implicit" cubeImplicit

    -- big fill
    
    let dil = -0.0001
    let bigFill = modify (Ra3.blank dil 0.5 ((-0.75, -0.5, -0.5), (2.1, 2.5, 2.5))) dil$
          Ra3.fillCubeE (0, 300)
    start <- trace "big-fill"$ getCPUTime
    end <- bigFill `deepseq` getCPUTime
    trace (printf "big-fill: %f" (((fromIntegral (end - start)) / (10^12)) :: Double))$ return ()

snowmanIO :: IO Ra3IO.Raster3
snowmanIO = do
  ra <- Ra3IO.blank 0 0.02 ((-1.5, -1.22, -1.35), (2.0, 1.22, 4.2))
  Ra3IO.modifyIO ra 0.0001$ Ra3IO.FloodFill$
    Diff [
      Ra3IO.toIO snowman
    , Ra3IO.floodFillE [(0.5, 0, -0.1)] (\(x, y, _) -> sqrt ((x - 0.5)^2 + y^2) - 0.2)
    , Ra3IO.floodShellE (-0.04, 0.02) [(0.1, 0.6, -0.1)] (\(_, y, _) -> 0.5 - y)
    ]
  return ra

snowmanST :: Raster3
snowmanST =
  modifyST (Ra3.blank 0 0.02 ((-1.5, -1.22, -1.35), (2.0, 1.22, 4.2))) 0.0001$ FloodFill$
    Diff [
      -- Ra3.floodFillE [(0, 0, 0)] (\(x, y, z) -> 1)  -- empty floodfill
      Ra3.toST snowman
    , Ra3.floodFillE [(0.5, 0, -0.1)] (\(x, y, _) -> sqrt ((x - 0.5)^2 + y^2) - 0.2)
    , Ra3.floodShellE (-0.04, 0.02) [(0.1, 0.6, -0.1)] (\(_, y, _) -> 0.5 - y)
    ]

snowmanRa :: Raster3
snowmanRa = modify (Ra3.blank 0 0.02 ((-1.5, -1.22, -1.35), (2.0, 1.22, 4.2))) (-0.0001)$ snowman

snowman =
  Union
    [ Diff
        [ Ra3.fillObjE$ sphere 1.2
        , Ra3.fillObjE$ rect3R 0 (0, 0, 0) (2, 2, 2)
        ]
    -- , translateE (0, 0.5, 2.1) <~ rotateE (0, -pi/2, pi/2) <~ Union 
    , (\(x, _, _) -> x > -0.5) </~ translateE (0, 0, 2.1) <~ Union 
        [ Diff
            [ Ra3.fillObjE$ sphere 1
            , Ra3.fillObjE$ rect3R 0 (-2.5) 0 -- FIXME: the cube is deleted from lower sphere as well...
            ]
        , Ra3.fillObjE
            $ rotate3 (0, pi/2, 0) 
                $ extrudeRM 0 (Left 0) (Right (\z -> 1-z*0.5)) (Left (0, 0)) (circle 0.3) (Left 2)
        , Ra3.fillObjE$ translate (0, 0, 1)
            $ extrudeOnEdgeOf 
                  (polygonR 0 [(0.3, 0), (0.3, 0.2), (0, 0.2), (0, 1), (-0.5, 1), (-0.5, 0)])
                $ circle(0.5)
        ]
    ]