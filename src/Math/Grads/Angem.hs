-- | Module providing miscellaneous functions for working with
-- coordinates, vectors and matrices.
--
module Math.Grads.Angem
  ( alignmentFunc
  , areIntersected
  , eqV2
  , rotation2D
  , reflectPoint
  ) where

import           Math.Grads.Angem.Internal.MatrixOperations (alignmentFunc,
                                                             rotation2D)
import           Math.Grads.Angem.Internal.VectorOperations (areIntersected,
                                                             eqV2, reflectPoint)
