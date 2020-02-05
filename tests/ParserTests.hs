{-# LANGUAGE OverloadedStrings #-}
module ParserTests
    ( spec
    ) where

import qualified Data.Text.IO              as Tx
import qualified Model.Types               as T
import qualified Model.Journals            as J
import qualified Model.Parsers.JournalSets as P
import qualified Model.References          as R
import qualified Viewer                    as V
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
testGenParse2019 = it "Parses JSet2019 to yearly26Sets 2019" $ do
    let journals = map T.journal R.issueRefs
        expected = V.tabulateJSets journals . J.yearly26Sets 2019 $ R.issueRefs
    etJSets <- P.parseJournalSets <$> Tx.readFile "tests/res/JSet2019.csv"
    case etJSets of
         Left err    -> error err
         Right jSets -> V.tabulateJSets journals jSets `shouldBe` expected
