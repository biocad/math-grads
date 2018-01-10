{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Math.Grads.Drawing.Coords (Drawable, getCoordsForGraph)
import           Math.Grads.GenericGraph   (GenericGraph)
import           Math.Grads.Graph          (fromList)
import           System.Random             (mkStdGen)
import           Test.Hspec

instance Drawable GenericGraph Int Int

pathToGraphs :: FilePath
pathToGraphs = "data/Graphs.txt"

roundPair :: (Float, Float) -> (Int, Int)
roundPair (a, b) = (round a, round b)

testMap :: IO (Map String (GenericGraph Int Int, Map Int (Int, Int)))
testMap = do
    graphsInLines <- lines <$> readFile pathToGraphs
    let graphsInWords = fmap words graphsInLines

    let forMap = fmap (\(x : y : z : _) -> (x, (fromList (read y), fmap roundPair (read z)))) graphsInWords
    return (M.fromList forMap)

testDrawing :: SpecWith ()
testDrawing = describe "Check whether molecules are being drawn correctly." $ do
    it "Only path" $ do
        (graph, coords) <- fmap (M.! "only_path") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Just coords
    it "Only cycles" $ do
        (graph, coords) <- fmap (M.! "only_cycles") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Just coords
    it "Simple drawing" $ do
        (graph, coords) <- fmap (M.! "simple_drawing") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Just coords
    it "Hard drawing" $ do
        (graph, coords) <- fmap (M.! "hard_drawing") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Just coords
    it "Paths through conjugated cycles" $ do
        (graph, coords) <- fmap (M.! "paths_through_conjugated_cycles") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Just coords

testErrors :: SpecWith ()
testErrors = describe "Check that coordinates for molecules that we can't draw are returned as Nothing." $ do
    it "Too big cycle" $ do
        (graph, _) <- fmap (M.! "too_big_cycle") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Nothing
    it "Bad conjugated cycle" $ do
        (graph, _) <- fmap (M.! "bad_conjugated_cycle") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Nothing
    it "Disappearing cycle" $ do
        (graph, _) <- fmap (M.! "disappearing_cycle") testMap
        (roundPair <$>) <$> getCoordsForGraph (mkStdGen 0) graph `shouldBe` Nothing

main :: IO ()
main = hspec $ do
  testDrawing
  testErrors
