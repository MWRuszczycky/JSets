module CoreTests
    ( spec
    ) where

import qualified Model.Core.Core as C
import Test.Hspec                       ( Spec (..)
                                        , hspec
                                        , it
                                        , describe
                                        , shouldBe )

spec :: IO ()
spec = hspec $ do
    describe "shuffleIn" $ do
        shuffleInSpec
    describe "shuffleInAt" $ do
        shuffleInAtSpec

shuffleInSpec :: Spec
shuffleInSpec = do
    let xs1 = "cat"
        xs2 = "dog"
        xs3 = "monkeys"
    it "handles empty lists" $ do
        C.shuffleIn []  xs2 `shouldBe` ""
        C.shuffleIn xs1 [] `shouldBe` ""
        C.shuffleIn []  [] `shouldBe` ""
    it "shuffles equal-length lists correctly" $ do
        C.shuffleIn xs1 xs2 `shouldBe` "cdaotg"
    it "shuffles unequal-length lists correctly" $ do
        C.shuffleIn xs1 xs3 `shouldBe` "cmaotn"
        C.shuffleIn xs3 xs1 `shouldBe` "mcoant"

shuffleInAtSpec :: Spec
shuffleInAtSpec = do
    let xs1 = "cat"
        xs2 = "dog"
        xs3 = "monkeys"
    it "handles empty lists" $ do
        C.shuffleInAt   1    1  []  [] `shouldBe` ""
        C.shuffleInAt   1    1  xs1 [] `shouldBe` ""
        C.shuffleInAt   1    1  [] xs2 `shouldBe` ""
        C.shuffleInAt (-1)   1  []  [] `shouldBe` ""
        C.shuffleInAt (-1)   1  xs1 [] `shouldBe` ""
        C.shuffleInAt (-1)   1  [] xs2 `shouldBe` "dog"
        C.shuffleInAt   1  (-1) []  [] `shouldBe` ""
        C.shuffleInAt   1  (-1) xs1 [] `shouldBe` "cat"
        C.shuffleInAt   1  (-1) [] xs2 `shouldBe` ""
    it "handles lists with nonpositive position" $ do
        C.shuffleInAt   0    1  xs1 xs2 `shouldBe` xs2
        C.shuffleInAt   1    0  xs1 xs2 `shouldBe` xs1
        C.shuffleInAt (-1)   1  xs1 xs2 `shouldBe` xs2
        C.shuffleInAt   1  (-1) xs1 xs2 `shouldBe` xs1
        C.shuffleInAt   0    0  xs1 xs2 `shouldBe` ""
    it "handles lists with positive position" $ do
        C.shuffleInAt 1 1 xs1 xs2 `shouldBe` "cdaotg"
        C.shuffleInAt 2 1 xs1 xs2 `shouldBe` "cad"
        C.shuffleInAt 3 1 xs1 xs2 `shouldBe` "catd"
        C.shuffleInAt 1 2 xs1 xs2 `shouldBe` "cdo"
        C.shuffleInAt 1 3 xs1 xs2 `shouldBe` "cdog"
        C.shuffleInAt 1 2 xs1 xs3 `shouldBe` "cmoanktey"
        C.shuffleInAt 2 1 xs3 xs2 `shouldBe` "modnkoeyg"
