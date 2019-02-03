-- | Some useful functions for operations with vectors.
--
module Math.Grads.Angem.Internal.VectorOperations
  ( areIntersected
  , avg
  , eqV2
  , reflectPoint
  ) where

import           Linear.Metric (distance, norm)
import           Linear.V2     (V2 (..))
import           Linear.Vector ((*^), (^+^), (^/))

-- | End of each line shouldn't be closer then this to other line.
--
eps :: Float
eps = 5

-- | Checks whether two lines intersect.
--
areIntersected :: (V2 Float, V2 Float) -> (V2 Float, V2 Float) -> Bool
areIntersected (x@(V2 x0 y0), y@(V2 x1 y1)) (x'@(V2 x0' y0'), y'@(V2 x1' y1')) = res
  where
    epsA = 20 -- Minimal distance between two lines

    a = x0 * y1 - y0 * x1
    b = x0' * y1' - x1' * y0'

    x01  = x0 - x1
    x01' = x0' - x1'
    y01  = y0 - y1
    y01' = y0' - y1'

    division = x01 * y01' - y01 * x01'

    px = (a * x01' - x01 * b) / division
    py = (a * y01' - y01 * b) / division

    notCommonPoint = not (x `eqV2` x' || x `eqV2` y' || y `eqV2` x' || y `eqV2` y')

    inXBounds = min x0 x1 - eps < px && px < max x0 x1 + eps && min x0' x1' - eps < px && px < max x0' x1' + eps
    inYBounds = min y0 y1 - eps < py && py < max y0 y1 + eps && min y0' y1' - eps < py && py < max y0' y1' + eps

    pointOnLine = pointBelongsToLine (x', y') x || pointBelongsToLine (x', y') y
                  || pointBelongsToLine (x, y) x' || pointBelongsToLine (x, y) y'
    notDistantEnough = not (norm (x - x') > epsA && norm (x - y') > epsA && norm (y - x') > epsA && norm (y - y') > epsA)

    res = notCommonPoint && (division /= 0 && inXBounds && inYBounds || pointOnLine || notDistantEnough)

-- | Reflects point over given line.
--
reflectPoint :: (V2 Float, V2 Float) -> V2 Float -> V2 Float
reflectPoint (coordA, coordB) thisPoint = res
  where
    V2 dirA dirB = coordB - coordA

    a' = V2 (-dirB) dirA
    a  = a' ^/ distance a' (V2 0.0 0.0)
    b' = V2 dirB (-dirA)
    b  = b' ^/ distance b' (V2 0.0 0.0)

    distanceFrom = distanceFromPointToLine (coordA, coordB)
    normVec = if distanceFrom (thisPoint + a) < distanceFrom (thisPoint + b) then a
              else b

    transform x = x + 2 * distanceFromPointToLine (coordA, coordB) x *^ normVec

    res = if pointBelongsToLine (coordA, coordB) thisPoint then thisPoint
          else transform thisPoint

distanceFromPointToLine :: (V2 Float, V2 Float) -> V2 Float -> Float
distanceFromPointToLine (V2 x1 y1, V2 x2 y2) (V2 x0 y0) = res
  where
    res = abs ((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1) / sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

pointBelongsToLine :: (V2 Float, V2 Float) -> V2 Float -> Bool
pointBelongsToLine (V2 x0 y0, V2 x1 y1) (V2 x' y') = (x0 * (x' - x1) + y0 * (y' - y1)) `eqFloat` 0.0 &&
 (min x0 x1 < x' && x' < max x0 x1 && min y0 y1 < y' && y' < max y0 y1)

-- | Given list of points calculates centroid of these points.
--
avg :: [V2 Float] -> V2 Float
avg points = foldl1 (^+^) points ^/ fromIntegral (length points)

-- | Checks two vectors of coordinates for equality.
--
eqV2 :: V2 Float -> V2 Float -> Bool
eqV2 (V2 a b) (V2 a' b') = a `eqFloat` a' && b `eqFloat` b'

-- TODO: We need to somehow consider length of line when comparing coordinates of two points
eqFloat :: Float -> Float -> Bool
eqFloat x y = abs (x - y) < eps
