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
    describe "extension" $ do
        extensionSpec
    describe "chunksOf" $ do
        chunksOfSpec
    describe "takeEveryAt" $ do
        takeEveryAtSpec
    describe "collate" $ do
        collateSpec
    describe "shuffleIn" $ do
        shuffleInSpec
    describe "shuffleInAt" $ do
        shuffleInAtSpec
    describe "zipLists" $ do
        zipListsSpec

extensionSpec :: Spec
extensionSpec = do
    it "Works on empty paths" $ do
        C.extension "" `shouldBe` []
    it "Works on paths with no extensions" $ do
        C.extension "/dir1/dir2/filename" `shouldBe` []
    it "Works on paths with a single extension" $ do
        C.extension "/dir1/dir2/filename.mkd" `shouldBe` "mkd"
    it "Works on paths with a multiple extensions" $ do
        C.extension "/dir1/dir2.dir/filename.mkd" `shouldBe` "mkd"
        C.extension "/dir1.cat/dir2.dir/file.name.cat.mkd" `shouldBe` "mkd"

chunksOfSpec :: Spec
chunksOfSpec = do
    let xs1 = "the yellow cat"
    it "handles empty lists" $ do
        C.chunksOf 2 "" `shouldBe` []
    it "handles nonpositive chunks" $ do
        C.chunksOf 0    xs1 `shouldBe` []
        C.chunksOf (-2) xs1 `shouldBe` []
    it "correctly appends overhang" $ do
        C.chunksOf 3  xs1 `shouldBe` [ "the", " ye", "llo", "w c", "at" ]
        C.chunksOf 20 xs1 `shouldBe` [ xs1 ]
    it "works correctly on a list with no overhang" $ do
        C.chunksOf 1 xs1 `shouldBe` map (:[]) xs1
        C.chunksOf 7 xs1 `shouldBe` [ "the yel", "low cat" ]

takeEveryAtSpec :: Spec
takeEveryAtSpec = do
    let xs1 = "the yellow cat is fat"
    it "handles empty lists" $ do
        C.takeEveryAt 1 2 "" `shouldBe` []
    it "handles nonpositive everies" $ do
        C.takeEveryAt 0    2 xs1 `shouldBe` []
        C.takeEveryAt (-2) 2 xs1 `shouldBe` []
    it "handles nonpositive ats" $ do
        C.takeEveryAt 2 0    xs1 `shouldBe` C.chunksOf 2 xs1
        C.takeEveryAt 3 0    xs1 `shouldBe` C.chunksOf 3 xs1
        C.takeEveryAt 2 (-2) xs1 `shouldBe` C.chunksOf 2 xs1
        C.takeEveryAt 3 (-2) xs1 `shouldBe` C.chunksOf 3 xs1
    it "handles overhang in the every" $ do
        C.takeEveryAt 6 3    xs1 `shouldBe` [ "the ye", "w cat ", "fat" ]
        C.takeEveryAt 6 4    xs1 `shouldBe` [ "the ye", " cat i", "t"   ]
    it "handles overhang in the at" $ do
        C.takeEveryAt 6 6    xs1 `shouldBe` [ "the ye", "at is "   ]
        C.takeEveryAt 7 5    xs1 `shouldBe` [ "the yel", "at is f" ]
    it "takes underhang in the every" $ do
        C.takeEveryAt 30 1   xs1 `shouldBe` [ xs1 ]
        C.takeEveryAt 22 2   xs1 `shouldBe` [ xs1 ]
    it "handles exact every-at" $ do
        C.takeEveryAt 3  4 xs1 `shouldBe` [ "the", "low", " is"        ]
        C.takeEveryAt 4  3 xs1 `shouldBe` [ "the ", "low ", " is "     ]
        C.takeEveryAt 5  2 xs1 `shouldBe` [ "the y", "low c", " is f"  ]
        C.takeEveryAt 21 2 xs1 `shouldBe` [ xs1                        ]
        C.takeEveryAt 20 1 xs1 `shouldBe` [ "the yellow cat is fa"     ]
        C.takeEveryAt 1  1 xs1 `shouldBe` [ "t", "e", "y", "l", "o", " "
                                          , "a", " ", "s", "f", "t"    ]

collateSpec :: Spec
collateSpec = do
    it "handles empty lists" $ do
        C.collate 2 ["", "bbbb", "cccc"] `shouldBe` ""
        C.collate 2 ([] :: [String])     `shouldBe` ""
        C.collate 2 ["aaaa", "", "cccc"] `shouldBe` ""
        C.collate 2 ["aaaa", "bbbb", ""] `shouldBe` ""
    it "handles underhang by returning an empty list" $ do
        C.collate 2 ["aaa", "bbb", "c"]  `shouldBe` ""
    it "handles nonpositive inputs" $ do
        C.collate 0    ["aaaa", "bbbb", "cccc"] `shouldBe` ""
        C.collate (-1) ["aaaa", "bbbb", "cccc"] `shouldBe` ""
        C.collate (-2) ["aaaa", "bbbb", "cccc"] `shouldBe` ""
    it "correctly handles overhang" $ do
        C.collate 3 ["aaaa", "bbbb", "cccc"]    `shouldBe` "aaabbbccc"
        C.collate 3 ["aaaa", "bbbb", "ccccc"]   `shouldBe` "aaabbbccc"
        C.collate 3 ["aaaa", "bbbb", "cccccc"]  `shouldBe` "aaabbbccc"
        C.collate 3 ["aaaa", "bbbbb", "ccc"]    `shouldBe` "aaabbbccc"
        C.collate 3 ["aaaa", "bbbbbb", "ccc"]   `shouldBe` "aaabbbccc"
        C.collate 3 ["aaaa", "bbbbbbb", "ccc"]  `shouldBe` "aaabbbccc"
        C.collate 2 ["aaaa", "bbbbbbb", "cccc"] `shouldBe` "aabbccaabbcc"
        C.collate 1 ["aaaa", "bbbbbbb", "cccc"] `shouldBe` "abcabcabcabc"
    it "correctly collates exact lists" $ do
        C.collate 2 ["aaaa", "bbbb", "cccc"] `shouldBe` "aabbccaabbcc"
        C.collate 1 ["aaaa", "bbbb", "cccc"] `shouldBe` "abcabcabcabc"

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

zipListsSpec :: Spec
zipListsSpec = do
    it "handles empty lists" $ do
        C.zipLists ["dog"] []   `shouldBe` []
        C.zipLists [] ["cat"]   `shouldBe` []
        C.zipLists [""] [""]    `shouldBe` [""]
        C.zipLists [""] ["cat"] `shouldBe` ["cat"]
        C.zipLists ["dog"] [""] `shouldBe` ["dog"]
    let xs = ["dog", "cat", "fish"]
        ys = ["monkey", "chicken"]
        zs = ["horse", "turtle", "mongoose"]
    it "handles overhang correctly" $ do
        C.zipLists xs ys `shouldBe` ["dogmonkey", "catchicken"]
        C.zipLists ys xs `shouldBe` ["monkeydog", "chickencat"]
    it "handles exact lists correctly" $ do
        C.zipLists xs zs `shouldBe` ["doghorse", "catturtle", "fishmongoose"]

