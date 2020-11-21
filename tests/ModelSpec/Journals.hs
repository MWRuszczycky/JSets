{-# LANGUAGE OverloadedStrings #-}
module ModelSpec.Journals
    ( spec
    ) where

import qualified Data.Time            as Tm
import qualified Data.Text.IO         as Tx
import qualified Model.Core.Types     as T
import qualified Mock.References      as TR
import qualified Mock.Mock            as Mock
import qualified Model.Journals       as J
import qualified Model.Parsers.PubMed as P
import qualified View.View            as V
import           Data.Ord                     ( comparing     )
import           Data.List                    ( sort, sortBy  )
import           Test.Hspec                   ( Spec (..)
                                              , hspec, it
                                              , describe
                                              , shouldSatisfy
                                              , shouldBe      )

spec :: IO ()
spec = hspec $ do
    describe "Model.Journals.issueAtDate" $ do
        spec_issueAtDate
    describe "Model.Journals.yearlySets" $ do
        spec_yearlySets

-- =============================================================== --
-- Working with journal sets

testRefs_Empty :: T.References
testRefs_Empty = []

testRefs_1W, testRefs_2W, testRefs_3W :: T.References
testRefs_1W = [ TR.biochemistryRef ]
testRefs_2W = [ TR.acieRef, TR.biochemistryRef ]
testRefs_3W = [ TR.acieRef, TR.biochemistryRef, TR.natureRef ]

testRefs_1M, testRefs_2M, testRefs_3M :: T.References
testRefs_1M = [ TR.natChemRef ]
testRefs_2M = [ TR.natChemRef, TR.natChemBiolRef ]
testRefs_3M = [ TR.cellChemBiolRef, TR.natChemRef, TR.natChemBiolRef ]

testRefs_1W1M :: T.References
testRefs_1W1M = [ TR.biochemistryRef, TR.natChemRef ]

check_yearly :: T.JSets T.Issue -> FilePath -> IO ()
check_yearly jsets path = do
    expected <- Tx.readFile $ "tests/res/" <> path
    Mock.runView ( V.jsetsTxt jsets ) `shouldBe` expected

---------------------------------------------------------------------
-- Model.Journals.yearlySets

spec_yearlySets :: Spec
spec_yearlySets = do
    it "works with no issues" $ do
        let T.JSets result = J.yearlySets 2019 2 testRefs_Empty
        result `shouldSatisfy` null
    it "works with 1 monthly issue at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_1M) "yearlySets_0W1M2F.txt"
    it "works with 2 monthly issues at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_2M) "yearlySets_0W2M2F.txt"
    it "works with 3 monthly issues at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_3M) "yearlySets_0W3M2F.txt"
    it "works with 1 weekly-first issue at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_1W) "yearlySets_1W0M2F.txt"
    it "works with 1 weekly & 1 weekly-first issues at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_2W) "yearlySets_2W0M2F.txt"
    it "works with 1 weekly, 1 weekly-first & 1 weekly-last at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_3W) "yearlySets_3W0M2F.txt"
    it "works with 1 monthly & 1 weekly-first issues at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 testRefs_1W1M) "yearlySets_1W1M2F.txt"
    it "works with 3 monthly & 6 weekly issues at 1 week freq" $ do
        check_yearly (J.yearlySets 2019 1 TR.issueRefs) "yearlySets_6W3M1F.txt"
    it "works with 3 monthly & 6 weekly issues at 2 week freq" $ do
        check_yearly (J.yearlySets 2019 2 TR.issueRefs) "yearlySets_6W3M2F.txt"
    it "works with 3 monthly & 6 weekly issues at 5 week freq" $ do
        check_yearly (J.yearlySets 2019 5 TR.issueRefs) "yearlySets_6W3M5F.txt"
    it "works with 3 monthly & 6 weekly issues at 52 week freq" $ do
        check_yearly (J.yearlySets 2019 52 TR.issueRefs) "yearlySets_6W3M52F.txt"

-- =============================================================== --
-- Working with journal issues

checkVolNum :: T.Issue -> Int -> Int -> IO ()
checkVolNum x v n = (T.volNo x, T.issNo x) `shouldBe` (v, n)

---------------------------------------------------------------------
-- Model.Journals.issueAtDate

spec_issueAtDate :: Spec
spec_issueAtDate = do
    test_IssuesForScience
    test_IssuesForJACS
    test_IssuesForCellChemBiol

test_IssuesForScience :: Spec
test_IssuesForScience = do
    it "identifies issues near Science reference correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2009 12 31) TR.scienceRef
            n2 = J.issueAtDate (Tm.fromGregorian 2010  1  1) TR.scienceRef
            n3 = J.issueAtDate (Tm.fromGregorian 2010  1  2) TR.scienceRef
            n4 = J.issueAtDate (Tm.fromGregorian 2010  5  6) TR.scienceRef
        checkVolNum n1 2010 5961
        checkVolNum n1 2010 5961
        checkVolNum n1 2010 5961
        checkVolNum n4 2010 5978
    it "identifies Science 2015-2016-2017 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2015 12 16) TR.scienceRef
            n2  = J.issueAtDate (Tm.fromGregorian 2015 12 18) TR.scienceRef
            n3  = J.issueAtDate (Tm.fromGregorian 2015 12 19) TR.scienceRef
            n4  = J.issueAtDate (Tm.fromGregorian 2016 1   1) TR.scienceRef
            n5  = J.issueAtDate (Tm.fromGregorian 2016 1   2) TR.scienceRef
            n6  = J.issueAtDate (Tm.fromGregorian 2016 6  10) TR.scienceRef
            n7  = J.issueAtDate (Tm.fromGregorian 2016 6  11) TR.scienceRef
            n8  = J.issueAtDate (Tm.fromGregorian 2016 12 20) TR.scienceRef
            n9  = J.issueAtDate (Tm.fromGregorian 2016 12 23) TR.scienceRef
            n10 = J.issueAtDate (Tm.fromGregorian 2016 12 31) TR.scienceRef
            n11 = J.issueAtDate (Tm.fromGregorian 2017 1   5) TR.scienceRef
            n12 = J.issueAtDate (Tm.fromGregorian 2017 1   6) TR.scienceRef
            n13 = J.issueAtDate (Tm.fromGregorian 2017 1   8) TR.scienceRef
        checkVolNum n1  2015 6266
        checkVolNum n2  2015 6266
        checkVolNum n3  2015 6267
        checkVolNum n4  2015 6267
        checkVolNum n5  2016 6268
        checkVolNum n6  2016 6290
        checkVolNum n7  2016 6291
        checkVolNum n8  2016 6318
        checkVolNum n9  2016 6318
        checkVolNum n10 2016 6319
        checkVolNum n11 2016 6319
        checkVolNum n12 2016 6319
        checkVolNum n13 2017 6320
    it "identifies Science 2018-2019-2020 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2018 12 18) TR.scienceRef
            n2  = J.issueAtDate (Tm.fromGregorian 2018 12 21) TR.scienceRef
            n3  = J.issueAtDate (Tm.fromGregorian 2019 1   4) TR.scienceRef
            n4  = J.issueAtDate (Tm.fromGregorian 2019 1   5) TR.scienceRef
            n5  = J.issueAtDate (Tm.fromGregorian 2019 6  13) TR.scienceRef
            n6  = J.issueAtDate (Tm.fromGregorian 2019 6  14) TR.scienceRef
            n7  = J.issueAtDate (Tm.fromGregorian 2019 6  15) TR.scienceRef
            n8  = J.issueAtDate (Tm.fromGregorian 2019 12 19) TR.scienceRef
            n9  = J.issueAtDate (Tm.fromGregorian 2019 12 20) TR.scienceRef
            n10 = J.issueAtDate (Tm.fromGregorian 2019 12 31) TR.scienceRef
            n11 = J.issueAtDate (Tm.fromGregorian 2020 1   2) TR.scienceRef
            n12 = J.issueAtDate (Tm.fromGregorian 2020 1   3) TR.scienceRef
            n13 = J.issueAtDate (Tm.fromGregorian 2020 1   6) TR.scienceRef
        checkVolNum n1  2018 6420
        checkVolNum n2  2018 6420
        checkVolNum n3  2018 6421
        checkVolNum n4  2019 6422
        checkVolNum n5  2019 6444
        checkVolNum n6  2019 6444
        checkVolNum n7  2019 6445
        checkVolNum n8  2019 6471
        checkVolNum n9  2019 6471
        checkVolNum n10 2019 6472
        checkVolNum n11 2019 6472
        checkVolNum n12 2019 6472
        checkVolNum n13 2020 6473

test_IssuesForJACS :: Spec
-- ^Actual JACS publication dates are not always consistent and can
-- come either 6 or 8 days, but average out to 7. For example, the
-- last issue of 2013 was on December 26, but it should have been on
-- December 25. Tests assume JACS is always 7 days between issues.
-- So, the December 25 day is used in the tests.
test_IssuesForJACS = do
    it "identifies issues near JACS reference correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2009 12 31) TR.jacsRef
            n2 = J.issueAtDate (Tm.fromGregorian 2010  1 13) TR.jacsRef
            n3 = J.issueAtDate (Tm.fromGregorian 2010  1 14) TR.jacsRef
            n4 = J.issueAtDate (Tm.fromGregorian 2010  5  6) TR.jacsRef
        checkVolNum n1 132 1
        checkVolNum n1 132 1
        checkVolNum n1 132 1
        checkVolNum n4 132 17
    it "identifies JACS 2013-2014-2015 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2013 12 24) TR.jacsRef
            n2  = J.issueAtDate (Tm.fromGregorian 2013 12 25) TR.jacsRef
            n3  = J.issueAtDate (Tm.fromGregorian 2013 12 31) TR.jacsRef
            n4  = J.issueAtDate (Tm.fromGregorian 2014 1   8) TR.jacsRef
            n5  = J.issueAtDate (Tm.fromGregorian 2014 1   9) TR.jacsRef
            n6  = J.issueAtDate (Tm.fromGregorian 2014 6  11) TR.jacsRef
            n7  = J.issueAtDate (Tm.fromGregorian 2014 6  12) TR.jacsRef
            n8  = J.issueAtDate (Tm.fromGregorian 2014 12 30) TR.jacsRef
            n9  = J.issueAtDate (Tm.fromGregorian 2014 12 31) TR.jacsRef
            n10 = J.issueAtDate (Tm.fromGregorian 2015 1   1) TR.jacsRef
            n11 = J.issueAtDate (Tm.fromGregorian 2015 1  13) TR.jacsRef
            n12 = J.issueAtDate (Tm.fromGregorian 2015 1  14) TR.jacsRef
            n13 = J.issueAtDate (Tm.fromGregorian 2015 1  15) TR.jacsRef
        checkVolNum n1  135 50
        checkVolNum n2  135 50
        checkVolNum n3  135 51
        checkVolNum n4  135 51
        checkVolNum n5  136  1
        checkVolNum n6  136 22
        checkVolNum n7  136 23
        checkVolNum n8  136 51
        checkVolNum n9  136 51
        checkVolNum n10 136 52
        checkVolNum n11 136 52
        checkVolNum n12 136 52
        checkVolNum n13 137  1
    it "identifies JACS 2015-2016-2017 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2015 12 29) TR.jacsRef
            n2  = J.issueAtDate (Tm.fromGregorian 2015 12 30) TR.jacsRef
            n3  = J.issueAtDate (Tm.fromGregorian 2015 12 31) TR.jacsRef
            n4  = J.issueAtDate (Tm.fromGregorian 2016 1  13) TR.jacsRef
            n5  = J.issueAtDate (Tm.fromGregorian 2016 1  14) TR.jacsRef
            n6  = J.issueAtDate (Tm.fromGregorian 2016 6   8) TR.jacsRef
            n7  = J.issueAtDate (Tm.fromGregorian 2016 6   9) TR.jacsRef
            n8  = J.issueAtDate (Tm.fromGregorian 2016 12 27) TR.jacsRef
            n9  = J.issueAtDate (Tm.fromGregorian 2016 12 28) TR.jacsRef
            n10 = J.issueAtDate (Tm.fromGregorian 2016 12 30) TR.jacsRef
            n11 = J.issueAtDate (Tm.fromGregorian 2017 1  10) TR.jacsRef
            n12 = J.issueAtDate (Tm.fromGregorian 2017 1  11) TR.jacsRef
            n13 = J.issueAtDate (Tm.fromGregorian 2017 1  12) TR.jacsRef
        checkVolNum n1  137 50
        checkVolNum n2  137 50
        checkVolNum n3  137 51
        checkVolNum n4  137 51
        checkVolNum n5  138  1
        checkVolNum n6  138 21
        checkVolNum n7  138 22
        checkVolNum n8  138 50
        checkVolNum n9  138 50
        checkVolNum n10 138 51
        checkVolNum n11 138 51
        checkVolNum n12 138 51
        checkVolNum n13 139  1

test_IssuesForCellChemBiol :: Spec
test_IssuesForCellChemBiol = do
    it "identifies issues near Cell Chem Biol reference correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2009 12 31) TR.cellChemBiolRef
            n2 = J.issueAtDate (Tm.fromGregorian 2010  1  1) TR.cellChemBiolRef
            n3 = J.issueAtDate (Tm.fromGregorian 2010  1  2) TR.cellChemBiolRef
            n4 = J.issueAtDate (Tm.fromGregorian 2010  5  6) TR.cellChemBiolRef
        checkVolNum n1 17 1
        checkVolNum n1 17 1
        checkVolNum n1 17 1
        checkVolNum n4 17 4
    it "identifies Cell Chem Biol 2015-2016-2017 issues correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2015 12 16) TR.cellChemBiolRef
            n2 = J.issueAtDate (Tm.fromGregorian 2016 1   1) TR.cellChemBiolRef
            n3 = J.issueAtDate (Tm.fromGregorian 2016 2   1) TR.cellChemBiolRef
            n4 = J.issueAtDate (Tm.fromGregorian 2016 6   1) TR.cellChemBiolRef
            n5 = J.issueAtDate (Tm.fromGregorian 2016 6  11) TR.cellChemBiolRef
            n6 = J.issueAtDate (Tm.fromGregorian 2016 12 23) TR.cellChemBiolRef
            n7 = J.issueAtDate (Tm.fromGregorian 2016 12 31) TR.cellChemBiolRef
            n8 = J.issueAtDate (Tm.fromGregorian 2017 1   5) TR.cellChemBiolRef
            n9 = J.issueAtDate (Tm.fromGregorian 2017 2   1) TR.cellChemBiolRef
        checkVolNum n1 22 11
        checkVolNum n2 22 12
        checkVolNum n3 23 1
        checkVolNum n4 23 5
        checkVolNum n5 23 5
        checkVolNum n6 23 11
        checkVolNum n7 23 11
        checkVolNum n8 23 12
        checkVolNum n9 24 1
