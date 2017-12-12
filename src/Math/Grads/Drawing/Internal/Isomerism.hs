module Drawing.Internal.Isomerism (DirIndList
                                 , DirInd
                                 , fixIso
                                 , getTetra) where

import           Data.List                  (delete, find, groupBy, nub, sortOn)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)

import           Linear.Metric              (dot)
import           Linear.V2                  (V2 (..))

import           Chemistry.Molecule         (Atom (..), EZIsomerism (..),
                                             Molecule (..), TetraDir (..),
                                             TetraIsomerism (..),
                                             getAtomWeightInMol,
                                             getMaxPriorities,
                                             moleculeToMolGraph)
import           Drawing.Internal.Coords    (coordListForDrawing, findBond,
                                             findBondInd)
import           Drawing.Internal.Utilities (Coord, CoordList, pairToV2,
                                             reflectBond)
import           Graph.Class                (toList, (!.))
import           Graph.EdgeVertexUtils      (getEnds)
import           Graph.Traversals           (dfs)
import           Math.Math                  (trueAngle)

-- Applies isomerism rules to molecule's isomerisms
-- fixIso :: Molecule -> CoordList -> [(MolBond, Char)] -> CoordList

type DirInd = (Int, TetraDir)
type DirIndList = [DirInd]

fixIso :: Molecule -> CoordList -> CoordList
fixIso mol cs = foldl (fixEZ mol) cs isoEZ
  where
    isoEZ = filter ((/= EZUnspec) . snd) (M.toList $ getEZIsomerism mol)

fixEZ :: Molecule -> CoordList -> (Int, EZIsomerism) -> CoordList
fixEZ molecule coords (idx, isomerism) = result
  where
    atoms = getAtoms molecule
    bonds = getBonds molecule

    bond = bonds !! idx
    bondItself@((left, right, _), (coordA, coordB)) = fromJust (find (\(bond', _) -> bond' == bond) coords)

    Just (leftMaxIdx, rightMaxIdx) = getMaxPriorities atoms bonds idx
    vecOfBestLeft = calcVecOfBestLeft (findBond coords left leftMaxIdx) left
    vecOfBestRight = calcVecOfBestLeft (findBond coords right rightMaxIdx) right

    toTheRightBonds = dfs (fst <$> delete bondItself coords) right
    toTheRightCoords = filter (\(x, _) -> x `elem` toTheRightBonds) coords
    toTheLeftCoords = filter (\(x, _) -> notElem x toTheRightBonds) coords

    direction = dot vecOfBestLeft vecOfBestRight
    result = resultFromDir direction isomerism

    resultFromDir :: Float -> EZIsomerism -> CoordList
    resultFromDir dir E | dir < 0 = coords
                        | otherwise = toTheLeftCoords ++ ((`reflectBond` (coordA, coordB)) <$> toTheRightCoords)
    resultFromDir dir _ | dir > 0 = coords
                        | otherwise = toTheLeftCoords ++ ((`reflectBond` (coordA, coordB)) <$> toTheRightCoords)

    calcVecOfBestLeft :: Coord -> Int -> V2 Float
    calcVecOfBestLeft ((a', _, _), (coordA', coordB')) ind = if a' == ind then coordA' - coordB' else coordB' - coordA'

getTetra :: Molecule -> CoordList -> (CoordList, DirIndList)
getTetra molecule coords = (coords, resDirs)
  where
    inds = [0..length (getAtoms molecule) - 1]
    resDirs' = catMaybes (fmap (getDir molecule coords) inds)
    resDirs = (\x -> helper (tail x) (head x)) <$> groupBy (\(a, _) (b, _) -> a == b) resDirs'

    helper :: DirIndList -> DirInd -> DirInd
    helper [] y              = y
    helper (x@(_, t) : xs) y = if t /= Unspecified then x else helper xs y

getDir :: Molecule -> CoordList -> Int -> Maybe DirInd
getDir molecule coords ind | valCond && isJust isomery = Just (getDir' molecule coords (fromJust isomery))
                           | valCond && (length uniqueNeighbors == 4 || length uniqueNeighbors == 3 && hCount == 1) = Just (coordToChange, Unspecified)
                           | otherwise = Nothing
  where
    molGraph = moleculeToMolGraph molecule
    tetras = getTetraIsomerism molecule
    isomery = if ind `elem` M.keys tetras then Just (ind, tetras M.! ind)
              else Nothing

    (atoms, _) = toList molGraph
    atomItself = atoms !! ind
    neighbors = nub (molGraph !. ind)
    inds = fst <$> neighbors

    hCount = atomHCount atomItself

    valCond = sum (fmap (fromEnum . snd) neighbors) + hCount == 4 -- We don't consider as chiral centers atoms with valence other than 4

    -- TODO: fix
    uniqueNeighbors = nub (fmap (getAtomWeightInMol molecule) inds)

    fromInd = minimum inds
    coordToChange = findBondInd (getBonds molecule) fromInd ind

getDir' :: Molecule -> CoordList -> (Int, TetraIsomerism) -> DirInd
getDir' molecule coords (ind, isoType) = (indToChange, resDir)
  where
    molGraph = moleculeToMolGraph molecule
    systemOfInds' = fst <$> nub (molGraph !. ind)
    fromInd = minimum systemOfInds'
    systemOfInds = delete fromInd systemOfInds'

    indToChange = findBondInd (getBonds molecule) fromInd ind
    coordToChange = findBond coords fromInd ind
    coordsMap = coordListForDrawing coords

    thisPoint = pairToV2 (coordsMap M.! ind)
    indAngles = (\x -> (x, trueAngle thisPoint (pairToV2 (coordsMap M.! x)))) <$> systemOfInds
    sortedIndAngs = sortOn snd indAngles

    k = length systemOfInds

    moves = if k >= 3
              then
                fromEnum ((-1) ^ (countMoves (fst <$> sortedIndAngs) `mod` 2) /= (-1 :: Integer) ^ (k - 1))
              else
                helper sortedIndAngs

    begOfBond = (minimum . getEnds . fst) coordToChange

    resDir | (isoType == CW && moves == 0 || moves /= 0 && isoType == CCW) && begOfBond == fromInd = BackBeg
           | (isoType == CW && moves == 0 || moves /= 0 && isoType == CCW) && begOfBond /= fromInd = BackEnd
           | (isoType == CCW && moves == 0 || moves /= 0 && isoType == CW) && begOfBond == fromInd = FrontBeg
           | (isoType == CCW && moves == 0 || moves /= 0 && isoType == CW) && begOfBond /= fromInd = FrontEnd
           | otherwise = error "Tetra isomerism finding error."

    countMoves :: [Int] -> Int
    countMoves []       = error "Count moves on empty list."
    countMoves [_]      = 0
    countMoves (x : xs) = countMoves' (x : xs) + countMoves xs

    countMoves' :: [Int] -> Int
    countMoves' [] = error "Count moves helper on empty list."
    countMoves' [_] = 0
    countMoves' (x : y : xs) = if x > y then 1 + countMoves (y : xs) else countMoves (y : xs)

    helper :: [(Int, Float)] -> Int
    helper [(a, angleA), (b, angleB)] | abs (angleB - angleA) <= pi && a <= b = 0
                                      | abs (angleB - angleA) >= pi && a <= b = 1
                                      | abs (angleB - angleA) < pi && a > b = 1
                                      | otherwise = 0
    helper _ = error "Can't calculate tetra isomery."
