module Drawing.Internal.Paths (getCoordsOfPath, findPaths) where

import           Data.List                             (delete, find, intersect,
                                                        maximumBy, union, (\\))
import           Data.Map.Strict                       (fromList, (!))
import           Data.Maybe                            (fromJust, isJust)
import           Data.Ord                              (comparing)

import           Chemistry.Molecule                    (BondList, MolBond)
import           Drawing.Internal.Coords               (Link, bondLength)
import           Drawing.Internal.CyclesPathsAlignment (bondsToAlignTo)
import           Drawing.Internal.Utilities            (Coord, CoordList,
                                                        findIncidentCoords,
                                                        tupleToList)
import           Graph.EdgeVertexUtils                 (getIndices,
                                                        getSharedVertex,
                                                        getVertexIncident,
                                                        isIncident)
import           Graph.Paths                           (dfsSearch,
                                                        findBeginnings)
import           Graph.Traversals                      (dfs)
import           Linear.V2                             (V2 (..))
import           Math.Math                             (alignmentFunc)

data Path = Path {
  pStart  :: Int,
  pFinish :: Int,
  pBonds  :: BondList
}

-- Calculates coordinates of path
getCoordsOfPath :: BondList -> CoordList
getCoordsOfPath bonds = fst (greedy pathWithCoords)
  where
    paths = splitPathIntoLongest bonds []
    pathsWithCoords = fmap (\Path { pBonds=bonds' } -> pathToCoords bonds') paths
    pathWithCoords = zip pathsWithCoords (getLinks paths [])

    getLinks :: [Path] -> [Int] -> [[Link]]
    getLinks (x : xs) taken =
      let
        links = findLinkingPoints x taken xs
      in links : getLinks xs (taken `union` fmap fst (countNeighbors links))
    getLinks _ _ = error "Linking point helper function on an empty list."

    greedy :: [(CoordList, [Link])] -> (CoordList, [Link])
    greedy [x]    = x
    greedy (x : xs) = greedy (uniteOnLinkingBonds x xs : filter (helper' x) xs)
    greedy _      = error "Greedy function on an empty list."

    helper' :: (CoordList, [Link]) -> (CoordList, [Link]) -> Bool
    helper' x' y' = null $ (snd <$> snd x') `intersect` (fst <$> fst y')

-- Takes path, list of vertices which have been processed and returns links for each path.
findLinkingPoints :: Path -> [Int] -> [Path] -> [(Int, MolBond)]
findLinkingPoints Path { pBonds=list } taken = helper
  where
    getInc :: Int -> BondList
    getInc n = filter (`isIncident` n) list

    couldTake :: Int -> Bool
    couldTake n = n `notElem` taken && (not . null $ getInc n)

    helper :: [Path] -> [Link]
    helper [] = []
    helper (Path beg end list' : xs) | couldTake beg = wrapResult beg list' : helper xs
                                     | couldTake end = wrapResult end list' : helper xs
                                     | otherwise = helper xs

    wrapResult :: Int -> BondList -> Link
    wrapResult n l = (fromJust $ getSharedVertex b1 b2, b2)
      where
        b1 = head $ getInc n
        b2 = head $ getVertexIncident l n

splitPathIntoLongest :: BondList -> [Int] -> [Path]
splitPathIntoLongest [] _ = []
splitPathIntoLongest bonds taken = firstPath : splitPathIntoLongest restBonds newTaken
  where
    firstPath = Path { pStart=start, pFinish=finish, pBonds=longestPath }
    restBonds = bonds \\ longestPath
    newTaken = taken `union` getIndices longestPath

    ends' = findBeginnings bonds
    ends = filter (`notElem` taken) ends'

    startEnds = if not (null taken) then taken else ends'

    allPaths = filter (not . null) ((\(x, y) -> maybe [] fst (dfsSearch bonds x y)) <$> allPairs startEnds ends)
    allPathsTrue = concatMap (\x -> maybe [x] (splitOnPoint x) (findPointsToSplit x taken)) allPaths

    longestPath = maximumBy (comparing length) allPathsTrue
    [start, finish] = findBeginnings longestPath

pathToCoords :: BondList -> CoordList
pathToCoords bonds = matchBondsOfPath coords bonds
  where
    angle = pi / 6.0
    radius = bondLength
    allPoints = getPoint <$> concat (repeat [1.0, -1.0])
    dfsRes = dfs bonds (head (findBeginnings bonds))
    coords = zip (getIndicesEdges dfsRes) (buildPath allPoints)

    getPoint :: Float -> V2 Float
    getPoint m = V2 (radius * cos (m * angle)) (radius * sin (m * angle))

    getIndicesEdges :: BondList ->  [Int]
    getIndicesEdges [] = error "Get indices edges on empy list."
    getIndicesEdges [(a, b, _)] = [a, b]
    getIndicesEdges (bnd@(a, b, _) : bnd'@(a', b', _) : xs) = if a == a' || a == b' then b : a : helper (bnd' : xs) bnd
                                                              else a : b : helper (bnd' : xs) bnd

    helper :: BondList -> MolBond -> [Int]
    helper [] _ = error "Get indices edges helper on empty list."
    helper [(a', b', _)] (a, b, _) = if a' == a || a' == b then [b']
                                              else [a']
    helper (bnd@(a, b, _) : bnd'@(a', b', _) : xs) _ = if a == a' || a == b' then a : helper (bnd' : xs) bnd
                                                       else b : helper (bnd' : xs) bnd

buildPath :: [V2 Float] -> [V2 Float]
buildPath [] = []
buildPath (y : ys) = V2 0.0 0.0 : y : helper ys y
  where
    helper :: [V2 Float] -> V2 Float -> [V2 Float]
    helper [] _ = []
    helper (b:xs) b' = let newCoords = b + b' in newCoords : helper xs newCoords

matchBondsOfPath :: [(Int, V2 Float)] -> BondList -> CoordList
matchBondsOfPath matches = helper
  where
    mapOfCoords = fromList matches

    helper :: BondList -> CoordList
    helper [] = []
    helper (bond@(a, b, _) : xs) = (bond, (mapOfCoords ! a, mapOfCoords ! b)) : helper xs

uniteOnLinkingBonds :: (CoordList, [Link]) -> [(CoordList, [Link])] -> (CoordList, [Link])
uniteOnLinkingBonds (mainPath, uniteThis) otherPaths = pathUniter coordsToAdd (mainPath, [])
  where
    counted = countNeighbors uniteThis
    neighbors = fmap getCoordsOfLinks counted

    coordsToAdd = concatMap align neighbors

    align :: (Int, [(V2 Float, V2 Float)]) -> [(CoordList, [Link])]
    align (ind, toAlignBonds) = alignOnBonds <$> zip toAlignBonds (findNeighbors otherPaths ind)

    getCoordsOfLinks :: (Int, Int) -> (Int, [(V2 Float, V2 Float)])
    getCoordsOfLinks (ind, counts) =
      let
        (leftBond, rightBond) = findAdjacent mainPath ind
      in (ind, bondsToAlignTo leftBond rightBond counts)

    pathUniter :: [(CoordList, [Link])] -> (CoordList, [Link]) -> (CoordList, [Link])
    pathUniter [] res                   = res
    pathUniter ((a, b) : xs) (resA, resB) = pathUniter xs (resA ++ a, resB ++ b)

countNeighbors :: [Link] -> [(Int, Int)]
countNeighbors list = zip allInds (fmap (numberOfNeighbors list 0) allInds)
  where
    allInds = linkingIndices list []

    linkingIndices :: [Link] -> [Int] -> [Int]
    linkingIndices [] res = res
    linkingIndices (x : xs) res = if fst x `elem` res then linkingIndices xs res
                                  else linkingIndices xs (fst x : res)

    numberOfNeighbors :: [Link] -> Int -> Int -> Int
    numberOfNeighbors [] acc _ = acc
    numberOfNeighbors (x : xs) acc ind = if fst x == ind then numberOfNeighbors xs (acc + 1) ind
                                         else numberOfNeighbors xs acc ind

splitOnMultiplePoints :: BondList -> [Int] -> [BondList]
splitOnMultiplePoints bonds cut = helper [bonds] cut []
  where
    helper :: [BondList] -> [Int] -> [BondList] -> [BondList]
    helper [] _ _ = error "Split on multiple points helper on empty list."
    helper lastCut [] res = res ++ lastCut
    helper (x : xs) (y : ys) res = if y `elem` getIndices x then helper (splitOnPoint x y ++ xs) ys res
                                   else helper xs (y:ys) (x:res)

splitOnPoint :: BondList -> Int -> [BondList]
splitOnPoint list point = filter (not . null) foundNeighbors
  where
    foundNeighbors = fmap splitter list

    splitter :: MolBond -> BondList
    splitter bond@(a, b, _) | a == point = bond : dfs (delete bond list) b
                            | b == point = bond : dfs (delete bond list) a
                            | otherwise = []

allPairs :: [Int] -> [Int] -> [(Int, Int)]
allPairs starts ends = concatMap (\start -> fmap (\end -> (start, end)) ends) starts

-- This function is used for splitting one path into substantial pieces during calculation of coordinates of this path.
findPointsToSplit :: BondList -> [Int] -> Maybe Int
findPointsToSplit _ [] = Nothing
findPointsToSplit [] _ = Nothing
findPointsToSplit [_] _ = Nothing
findPointsToSplit list taken = helper ((tail . init) list)
  where
    helper :: BondList -> Maybe Int
    helper [] = Nothing
    helper ((a, b, _) : xs) | a `elem` taken = Just a
                            | b `elem` taken = Just b
                            | otherwise = helper xs

-- This function is used for splitting one path into several paths if one vertex of path belongs to cycle.
findPointsToSplitHard :: BondList -> [Int] -> [Int]
findPointsToSplitHard _ [] = []
findPointsToSplitHard [] _ = []
findPointsToSplitHard [_] _ = []
findPointsToSplitHard list taken = helper list []
  where
    helper :: BondList -> [Int] -> [Int]
    helper [] acc = acc
    helper (x@(a, b, _):xs) acc  | a `notElem` acc && a `elem` taken && hasNeighbor a x = helper xs (a : acc)
                                 | b `notElem` acc && b `elem` taken && hasNeighbor b x = helper xs (b : acc)
                                 | otherwise = helper xs acc

    hasNeighbor :: Int -> MolBond ->  Bool
    hasNeighbor ind x = isJust (find (\bond -> bond /= x && isIncident bond ind) list)

-- Finds all paths between cycles in graph
findPaths :: BondList -> [Int] -> [BondList]
findPaths [] _ = []
findPaths bonds [] = [bonds]
findPaths bonds taken = allPathsTrue
  where
    paths = findPaths' bonds
    allPathsTrue = concatMap (\x -> let cut = findPointsToSplitHard x taken in splitOnMultiplePoints x cut) paths

findPaths' :: BondList -> [BondList]
findPaths' [] = []
findPaths' bonds@(x : _) = newPath : findPaths' (filter (`notElem` newPath) bonds)
  where
    newPath = findPath bonds bonds [x] x

findPath :: BondList -> BondList -> BondList -> MolBond -> BondList
findPath [] _ found _ = found
findPath (x@(a, b, _) : xs) bonds found pathFrom@(src, dst, _) = if cond then findPath xs bonds newFound pathFrom
                                                                 else findPath xs bonds found pathFrom
  where
    cond = (a == dst || b == src || a == src || b == dst) && (x `notElem` found)
    newFound = findPath bonds bonds (found ++ [x]) x

alignOnBonds :: ((V2 Float, V2 Float), ((V2 Float, V2 Float), (CoordList, [(Int, MolBond)]))) -> (CoordList, [(Int, MolBond)])
alignOnBonds (coordsA, (coordsB, (list, toSave))) = (resCoords, toSave)
  where
    align = alignmentFunc (tupleToList coordsA) (tupleToList coordsB)
    resCoords = fmap (\(bond, (coordA', coordB')) -> (bond, (align coordA', align coordB'))) list

findNeighbors :: [(CoordList, [Link])] -> Int -> [((V2 Float, V2 Float), (CoordList, [Link]))]
findNeighbors [] _ = []
findNeighbors (x : xs) ind = if not (null found) then (fromJust (head found), x) : findNeighbors xs ind
                             else findNeighbors xs ind
  where
    found = filter isJust (fmap coordsOfAdjacentBond (fst x))

    coordsOfAdjacentBond :: Coord -> Maybe (V2 Float, V2 Float)
    coordsOfAdjacentBond ((a, b, _), (coordsA, coordsB)) | a == ind = Just (coordsA, coordsB)
                                                         | b == ind = Just (coordsB, coordsA)
                                                         | otherwise = Nothing

findAdjacent :: CoordList -> Int -> (Coord, Coord)
findAdjacent list ind = (leftNeighbor, rightNeighbor)
  where
    [leftNeighbor, rightNeighbor] = take 2 (findIncidentCoords ind list)
