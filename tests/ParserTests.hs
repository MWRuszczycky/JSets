{-# LANGUAGE OverloadedStrings #-}
module ParserTests
    ( spec
    ) where

import qualified Data.Text.IO              as Tx
import qualified Model.Core.Types          as T
import qualified Model.Journals            as J
import qualified Model.Parsers.JournalSets as P
import qualified View.View                 as V
import qualified Mock                      as Mock
import qualified TestReferences            as TR
import           Test.Hspec                       ( Spec (..)
                                                  , hspec
                                                  , it
                                                  , describe
                                                  , shouldBe )

spec :: IO ()
spec = hspec $ do
    describe "Journal set parsing and generation"$ do
        testGenParse2019

testGenParse2019 :: Spec
testGenParse2019 = do
    let refs       = TR.issueRefs
        yearly2019 = J.yearly26Sets 2019 refs
        abbrs      = map (T.abbr . T.journal) refs
        viewCsv    = Mock.runView . V.jsetsToCsv abbrs $ yearly2019
    it "views yearly26Sets correctly as csv" $ do
        expectedCsv <- Tx.readFile "tests/res/JSet2019.csv"
        viewCsv `shouldBe` expectedCsv
    it "parses yearly26Sets correctly as csv" $ do
        etJSets <- P.parseJSets refs <$> Tx.readFile "tests/res/JSet2019.csv"
        case etJSets of
             Left err    -> error err
             Right jsets -> Mock.runView (V.jsetsToCsv abbrs jsets)
                            `shouldBe` viewCsv
