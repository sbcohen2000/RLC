{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
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
randomMat :: IO (M22 Double)
randomMat = V2 <$> randomVector <*> randomVector

scalingMat :: V2 Double -> M33 Double
scalingMat (V2 sx sy) = V3 (V3 sx 0 0) (V3 0 sy 0) (V3 0 0 1)

translationMat :: V2 Double -> M33 Double
translationMat (V2 x y) = V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

rotationMat :: Double -> M33 Double
rotationMat theta = V3
  (V3 c (-s) 0)
  (V3 s    c 0)
  (V3 0    0 1)
  where s = sin theta
        c = cos theta

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
  { vector_color          :: !Color
  , vector_arrowhead_size :: !Double
  , vector_stroke_weight  :: !Double
  }

defaultVectorSettings :: VectorSettings
defaultVectorSettings =
  VectorSettings
  { vector_color = black
  , vector_arrowhead_size = 0
  , vector_stroke_weight = 0.01
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
drawVector s p v = withNewContext $ do
  setWidth (vector_stroke_weight s)
  strokeColor (vector_color s)
  beginPath (complexOfPoint p)
  addLineToPath (complexOfPoint (p .+^ v .-^ arrowheadSizeOffset))
  strokePath
  arrowhead s (p .+^ v) v
  where
    -- A vector which points in the direction of @v@, and has length
    -- equal to the size of the arrowhead.
    arrowheadSizeOffset =
      normalize v * let t = vector_arrowhead_size s in V2 t t

extendVectorToLength :: Double -> V2 Double -> V2 Double
extendVectorToLength l = (* V2 l l) . normalize

-- | Produce a HPDF `Matrix` from an `M33` of `Double`s.
matrixOfM3 :: M33 Double -> Matrix
matrixOfM3 (V3 (V3 a b c) (V3 d e f) _) = Matrix a d b e c f

-- | Find the component of @a@ projected onto @b@ which is
-- perpendicular to @a@.
projectPerp :: V2 Double -> V2 Double -> V2 Double
projectPerp a = project (perp a)

-- | Draw a grid with side-length of @size@ and @density@ grid lines
-- per-side (for the identity transformation), but under the transform
-- given by @m@, along with the given @pic@.
--
-- Because @m@ might sqeeze the axes, more than @density@ grid-lines
-- might be drawn.
drawPictureWithGrid :: Double -> M22 Double -> Draw () -> Draw ()
drawPictureWithGrid size m pic = do
  withNewContext $ do
    addShape clipRect
    setAsClipPath

    let xform = matrixOfM3 $
          -- Translate to the center of the frame
          translationMat halfSize
          -- Correct for the y-coordinates pointing down in the outer
          -- context.
          !*! scalingMat (V2 1 (-1))
          -- Scale the picture up since the picture coordinates are on
          -- [-1, 1] independent of the picture's document size.
          !*! scalingMat halfSize
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

    -- Draw the picture
    pic

  withNewContext $ do
    strokeColor black
    addShape clipRect
    strokePath

  where
    -- Basis vectors of @m@.
    b0, b1 :: V2 Double
    b0 = m ^.column _x
    b1 = m ^.column _y

    n0, n1 :: Int
    -- To determine how many grid lines we can fit along the axis, we
    -- find a vector perpendicular to the grid lines for a basis
    -- (using `projectPerp`), and count how many vectors we can fit in
    -- the square.
    n0 = ceiling (r / norm (projectPerp b1 b0))
    -- How many basis vectors can we fit along the 1st axis?
    n1 = ceiling (r / norm (projectPerp b0 b1))

    r :: Double
    r = sqrt 2

    clipRect = Rectangle (0 :+ 0) (size :+ size)

    halfSize = V2 (size / 2) (size / 2)

translated :: V2 Double -> Draw () -> Draw ()
translated v m = withNewContext (applyMatrix (matrixOfM3 (translationMat v)) >> m)

document :: M22 Double -> PDF ()
document m = do
  page1 <- addPage Nothing
  drawWithPage page1 $ do
    -- Use a more traditional coordinate frame with the upper-left
    -- corner of the document as the origin, and positive y pointing
    -- down.
    applyMatrix $ matrixOfM3 $ translationMat (V2 0 400) !*! scalingMat (V2 1 (-1))

    translated (V2 10 10) $ do
      drawPictureWithGrid 100 m $ do
        -- Basis vectors of @m@.
        let b0 = m ^.column _x
        let b1 = m ^.column _y

        let withArrowhead =
              defaultVectorSettings { vector_arrowhead_size = 0.1
                                    , vector_stroke_weight = 0.03 }
        drawVector (withArrowhead { vector_color = red   }) (P (V2 0 0)) b0
        drawVector (withArrowhead { vector_color = green }) (P (V2 0 0)) b1

    translated (V2 120 10) $ do
      drawPictureWithGrid 100 m (pure ())

main :: IO()
main = do
  let m = (scalingMat 0.2 !*! rotationMat 0.1) ^. _m22
  let rect = PDFRect 0 0 600 400
  runPdf "out.pdf" (standardDocInfo
                    { author = "Sam Cohen"
                    , compressed = False }) rect $ do
    document m
