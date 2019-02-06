-- | Functions for working with coordinates' alignment and matrix rotations.
--
module Math.Grads.Angem.Internal.MatrixOperations
  ( alignmentFunc
  , rotation2D
  ) where

import           Linear.Matrix                              (M22, det22,
                                                             transpose, (!*!),
                                                             (*!))
import           Linear.V2                                  (V2 (..))
import           Linear.Vector                              (negated, (^+^),
                                                             (^-^))
import           Math.Grads.Angem.Internal.VectorOperations (avg)

-- | Given two lists of points produces function that transforms coordinates of given point
-- according to allignment of first list of points on second.
--
alignmentFunc :: [V2 Float] -> [V2 Float] -> V2 Float -> V2 Float
alignmentFunc points1 points2 = transformFunc
  where
    (rotationM, transitionV) = superImpose points1 points2
    transformFunc = transform rotationM transitionV

superImpose :: [V2 Float] -> [V2 Float] -> (M22 Float, V2 Float)
superImpose points1 points2 = (rotation, transition)
  where
    (avg1, moved1) = moveToCenter points1
    (avg2, moved2) = moveToCenter points2
    aMatrix = transpose moved2 !*! moved1
    (u, vt) = svd aMatrix

    rotation' = rotationMatrix vt u
    rotation = if det22 rotation' >= 0
      then rotation'
      else case vt of
        (V2 v1 v2) -> rotationMatrix (V2 v1 (negated v2)) u

    transition = avg1 - (avg2 *! rotation)

svd :: M22 Float -> (M22 Float, M22 Float)
svd aMatrix' = (doubleToFloatM22 rotationA, doubleToFloatM22 rotationB)
  where
    V2 (V2 a b) (V2 c d) = floatToDoubleM22 aMatrix'
    e = (a + d) / 2
    f = (a - d) / 2
    g = (c + b) / 2
    h = (c - b) / 2
    q = sqrt (e ** 2 + h ** 2)
    r = sqrt (f ** 2 + g ** 2)
    a1 = atan2 g f
    a2 = atan2 h e
    sy = q - r
    s = if sy < 0 then -1 else 1
    theta = (a2 - a1) / 2
    phi = (a2 + a1) / 2

    rotationA = V2 (V2 (cos phi) (- s * sin phi)) (V2 (sin phi) (s * cos phi))
    rotationB = V2 (V2 (cos theta) (- sin theta)) (V2 (sin theta) (cos theta))

moveToCenter :: [V2 Float] -> (V2 Float, [V2 Float])
moveToCenter points = (avgPoint, movedPoints)
  where
    avgPoint = avg points
    movedPoints = (^-^ avgPoint) <$> points

rotationMatrix :: M22 Float -> M22 Float -> M22 Float
rotationMatrix vt u = transpose $ transpose vt !*! transpose u

-- | Given angle in degrees produces rotation matrix that corresponds to that angle.
--
rotation2D :: Float -> M22 Float
rotation2D angle = V2 (V2 (cos trueAngle) (- sin trueAngle)) (V2 (sin trueAngle) (cos trueAngle))
  where
    trueAngle = 2 * pi * angle / 360.0

transform :: M22 Float -> V2 Float -> V2 Float-> V2 Float
transform rotationM transitionV = convFunc
  where
    convFunc = transformVector rotationM transitionV

transformVector :: M22 Float -> V2 Float -> V2 Float-> V2 Float
transformVector rotationM transitionV v = (v *! rotationM) ^+^ transitionV

doubleToFloatM22 :: M22 Double -> M22 Float
doubleToFloatM22 (V2 a' b') = V2 (realToFrac <$> a') (realToFrac <$> b')

floatToDoubleM22 :: M22 Float -> M22 Double
floatToDoubleM22 (V2 a' b') = V2 (realToFrac <$> a') (realToFrac <$> b')
