{-# LANGUAGE OverloadedStrings #-}
module ModelSpec.Matching
    ( spec
    ) where

import qualified Model.Matching as Mt
import           Test.Hspec           ( Spec (..)
                                      , hspec, it
                                      , describe
                                      , shouldBe      )

spec :: IO ()
spec = hspec $ do
    describe "Model.Matching.score" $ do
        spec_score

---------------------------------------------------------------------
-- Model.Matching.score

spec_score :: Spec
spec_score = do
    let r1 = [ [1], [5,9,2], [3,11,4], [6], [7,13], [8] ]
        r2 = [ [1], [5], [9], [2], [3], [11], [4], [6], [7], [13], [8] ]
        e1 = [ (1,4), (2,3), (3,1), (4,1) ]
        e2 = [ (1,1), (2,1), (3,1), (4,1) ]
        e3 = [ (5,7), (9,7), (2,7), (3,6), (11,6), (6,2), (8,1) ]
        e4 = [ (5,9), (9,9), (2,9), (3,8), (11,8), (10,3), (12,3), (6,2), (8,1) ]
    it "works with no ranked papers" $ do
        Mt.score [1,2,3,4] [] `shouldBe` e2
    it "works with no indexed papers" $
        Mt.score [] r1 `shouldBe` []
    it "works with a single paper ranked that is not an indexed paper" $ do
        Mt.score [1,2,3,4] [[6]] `shouldBe` e2
    it "works with a single paper ranked that is an indexed paper" $ do
        Mt.score [1,2,3,4] [[3]] `shouldBe` [ (3,4), (1,1), (2,1), (4,1) ]
    it "works with a 4 indexed papers and only 3 are ranked (equally)" $ do
        Mt.score [1,2,3,4] [[3,4,1]] `shouldBe` [ (3,4), (4,4), (1,4), (2,1) ]
    it "works with 4 indexed papers and 11 singly ranked papers" $ do
        Mt.score [2,4,5,9] r2 `shouldBe` [ (5,4), (9,3), (2,2), (4,1) ]
    it "works with mixed rank-lists and different sets of indexed papers" $ do
        Mt.score [1,2,3,4]              r1 `shouldBe` e1
        Mt.score [2,5,9,3,11,6,8]       r1 `shouldBe` e3
        Mt.score [2,5,9,3,11,6,8,10,12] r1 `shouldBe` e4
