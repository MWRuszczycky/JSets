module ModelSpec.Core
    ( spec
    ) where

import qualified Model.Core.Core      as C
import qualified Model.Core.Hungarian as H
import           Test.Hspec                ( Spec (..)
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
    describe "hungarian" $ do
        hungarianSpec

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

---------------------------------------------------------------------
-- Hungarian algorithm tests

type TestSet = ( Int, [((Int,Int), Int)] )
type Solver  = [((Int,Int),Int)] -> Either String (Int, [(Int,Int)])

hungarianSpec :: Spec
hungarianSpec = do
    it "Maximizes matchings for valid inputs" $ do
        runHTest test1max H.solveMax
        runHTest test2max H.solveMax
        runHTest test3max H.solveMax
        runHTest test4max H.solveMax
    it "Minimizes matchings for valid inputs" $ do
        runHTest test1min H.solveMin
        runHTest test2min H.solveMin
        runHTest test3min H.solveMin
        runHTest test4min H.solveMin

runHTest :: TestSet -> Solver -> IO ()
runHTest (expected, ws) solver = do
    case solver ws of
         Left err    -> error err
         Right (x,_) -> x `shouldBe` expected

test1max, test2max, test3max, test4max :: TestSet
test1max = (271, test1)
test2max = (5, test2)
test3max = (16, test3)
test4max = (437, test4)

test1min, test2min, test3min, test4min :: TestSet
test1min = (112, test1)
test2min = (5, test2)
test3min = (0, test3)
test4min = (124, test4)

test1 :: [((Int,Int), Int)]
test1 = [ ( (1,5), 42 ), ( (1,6), 53 ), ( (1,7), 53 ), ( (1,8),  7 )
        , ( (2,5), 94 ), ( (2,6), 70 ), ( (2,7), 52 ), ( (2,8), 21 )
        , ( (3,5), 78 ), ( (3,6), 82 ), ( (3,7), 47 ), ( (3,8), 72 )
        , ( (4,5), 31 ), ( (4,6),  2 ), ( (4,7), 43 ), ( (4,8), 42 )
        ]

test2 :: [((Int,Int),Int)]
test2 = [ ((1,3), 1), ((1,4), 2)
        , ((2,3), 3), ((2,4), 4)
        ]

test3 :: [((Int,Int),Int)]
test3 = [ ((1,4), 1), ((1,5), 6), ((1,6), 0)
        , ((2,4), 0), ((2,5), 8), ((2,6), 6)
        , ((3,4), 4), ((3,5), 0), ((3,6), 1)
        ]

test4 :: [((Int,Int), Int)]
test4 = [ ( (1,7), 30 ), ( (1,8), 44 ), ( (1,9), 14 ), ( (1,10), 67 ), ( (1,11), 67 ), ( (1,12), 92 )
        , ( (2,7), 10 ), ( (2,8), 50 ), ( (2,9), 22 ), ( (2,10), 31 ), ( (2,11), 52 ), ( (2,12), 53 )
        , ( (3,7), 55 ), ( (3,8), 19 ), ( (3,9), 54 ), ( (3,10), 36 ), ( (3,11), 13 ), ( (3,12), 86 )
        , ( (4,7), 39 ), ( (4,8), 52 ), ( (4,9),  4 ), ( (4,10), 63 ), ( (4,11), 10 ), ( (4,12), 81 )
        , ( (5,7), 86 ), ( (5,8), 28 ), ( (5,9), 82 ), ( (5,10), 72 ), ( (5,11), 85 ), ( (5,12), 82 )
        , ( (6,7), 60 ), ( (6,8), 58 ), ( (6,9), 43 ), ( (6,10), 99 ), ( (6,11), 43 ), ( (6,12), 26 )
        ]
