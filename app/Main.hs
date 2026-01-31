{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Graphics.PDF hiding (identity, project, Point)
import Linear.Affine
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import System.Random

gray :: Color
gray = Rgb 0.5 0.5 0.5

-- | Produce a random real-valued vector whose entries are on [0, 1].
randomVector :: IO (V2 Double)
randomVector = V2 <$> randomIO <*> randomIO

-- | Produce a random real-valued matrix whose entries are on [0, 1].
randomMatrix :: IO (M22 Double)
randomMatrix = V2 <$> randomVector <*> randomVector

-- | Re-interpret the first and second components of a `Point V2` as
-- the real and imaginary components of a `Complex`.
complexOfPoint :: Point V2 a -> Complex a
complexOfPoint (P (V2 a b)) = a :+ b

drawSeg :: Color -> V2 (Point V2 Double) -> Draw ()
drawSeg c (V2 a b) = do
  strokeColor c
  beginPath (complexOfPoint a)
  addLineToPath (complexOfPoint b)
  strokePath

-- | Transform a point on [-1, 1] to document space (a point on [0, size])
pointToDocSpace :: Double -> Point V2 Double -> Point V2 Double
pointToDocSpace size p = p * P halfSize .+^ halfSize
  where halfSize = V2 (size / 2) (size / 2)

data VectorSettings
  = VectorSettings
  { vector_color   :: Color
  , vector_arrowhead_size :: Double
  }

defaultVectorSettings :: VectorSettings
defaultVectorSettings =
  VectorSettings
  { vector_color = black
  , vector_arrowhead_size = 0
  }

arrowhead :: VectorSettings -> Point V2 Double -> V2 Double -> Draw ()
arrowhead s o v = do
  addShape $ Polygon
    [ complexOfPoint o
    , complexOfPoint (o .+^ a)
    , complexOfPoint (o .+^ b) ]
  fillColor (vector_color s)
  fillPath
  where
    a = back .+^ right
    b = back .+^ left

    back  =       -vn
    right =   perp vn
    left  = -(perp vn)

    vn = normalize v * V2 sz sz
    sz = vector_arrowhead_size s

-- | Draw a vector whose tail is at the given point and which points
-- in the given direction.
drawVector :: VectorSettings -> Point V2 Double -> V2 Double -> Draw ()
drawVector s p v = do
  strokeColor (vector_color s)
  beginPath (complexOfPoint p)
  addLineToPath (complexOfPoint (p .+^ v))
  strokePath
  arrowhead s (p .+^ v) v

extendVectorToLength :: Double -> V2 Double -> V2 Double
extendVectorToLength l = (* V2 l l) . normalize

-- | Produce a HPDF `Matrix` from an `M33` of `Double`s.
matrixOfM3 :: M33 Double -> Matrix
matrixOfM3 (V3 (V3 a b c) (V3 d e f) _) = Matrix a d b e c f

scalingMat :: V2 Double -> M33 Double
scalingMat (V2 sx sy) = V3 (V3 sx 0 0) (V3 0 sy 0) (V3 0 0 1)

translationMat :: V2 Double -> M33 Double
translationMat (V2 x y) = V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

-- | Find the component of @a@ projected onto @b@ which is
-- perpendicular to @a@.
projectPerp :: V2 Double -> V2 Double -> V2 Double
projectPerp a = project (perp a)

-- | Draw a grid with side-length of @size@ and @density@ grid lines
-- per-side (for the identity transformation), but under the
-- transform given by @m@.
--
-- Because @m@ might sqeeze the axes, more than @density@ grid-lines
-- might be drawn.
drawMatrixGrid :: Double -> M22 Double -> Draw ()
drawMatrixGrid size m = do
  strokeColor black
  addShape clipRect
  strokePath

  addShape clipRect
  setAsClipPath

  let xform = matrixOfM3 $ translationMat halfSize !*! scalingMat halfSize
  applyMatrix xform
  setWidth (1 / size)

  forM_ [(-n0)..n0] $ \i ->
    forM_ [(-n1)..n1] $ \j -> do
    let ii = fromIntegral <$> V2 i i
    let jj = fromIntegral <$> V2 j j

    -- Origin of the grid intersection
    let o = P $ (b0 * ii) .+^ (b1 * jj)

    drawVector defaultVectorSettings { vector_color=gray } o b0
    drawVector defaultVectorSettings { vector_color=gray } o b1

  where
    v0, v1, b0, b1 :: V2 Double
    -- Basis vectors in screen space
    v0 = V2 1 0
    v1 = V2 0 1

    -- Basis vectors under `m`
    b0 = m !* v0
    b1 = m !* v1

    n0, n1 :: Int
    -- To determine how many grid lines we can fit along the axis, we
    -- find a vector perpendicular to the grid lines for a basis
    -- (using `projectPerp`), and count how many vectors we can fit in
    -- the square.
    n0 = round (r / norm (projectPerp b1 b0))
    -- How many basis vectors can we fit along the 1st axis?
    n1 = round (r / norm (projectPerp b0 b1))

    r :: Double
    r = sqrt 2

    clipRect = Rectangle (0 :+ 0) (size :+ size)

    halfSize = V2 (size / 2) (size / 2)

document :: M22 Double -> PDF ()
document m = do
  page1 <- addPage Nothing
  drawWithPage page1 $ do
    drawMatrixGrid 100 m

main :: IO()
main = do
  -- let m = V2 (V2 1 0)
--             (V2 0 1)
  m <- randomMatrix
  let rect = PDFRect 0 0 600 400
  runPdf "out.pdf" (standardDocInfo
                    { author = "Sam Cohen"
                    , compressed = False }) rect $ do
    document m
