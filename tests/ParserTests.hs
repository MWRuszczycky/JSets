{-# LANGUAGE OverloadedStrings #-}
module ParserTests
    ( spec
    ) where

import qualified Data.Text.IO              as Tx
import qualified Model.Core.Types          as T
import qualified Model.Journals            as J
import qualified Model.Parsers.JournalSets as P
import qualified Model.Text.Formatting     as F
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
testGenParse2019 = it "Parses JSet2019 to yearly26Sets 2019" $ do
    let refs     = TR.issueRefs
        journals = map (T.key . T.journal) refs
        expected = F.jsetsToCsv journals . J.yearly26Sets 2019 $ refs
    etJSets <- P.parseJsets refs <$> Tx.readFile "tests/res/JSet2019.csv"
    case etJSets of
         Left err    -> error err
         Right jSets -> F.jsetsToCsv journals jSets `shouldBe` expected
