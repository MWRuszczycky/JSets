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
    describe "Model.Journals.yearly26Sets" $ do
        spec_yearly26Sets
    describe "Model.Journals.addCitations" $ do
        spec_addCitations
    describe "Model.Journals.missingPMIDs" $ do
        spec_missingPMIDs
    describe "Model.Journals.score" $ do
        spec_score

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
    Mock.runView ( V.jsetsIssueTxt jsets ) `shouldBe` expected

---------------------------------------------------------------------
-- Model.Journals.yearly26Sets

spec_yearly26Sets :: Spec
spec_yearly26Sets = do
    it "works with no issues" $ do
        let T.JSets result = J.yearly26Sets 2019 testRefs_Empty
        result `shouldSatisfy` null
    it "works with one monthly issue" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_1M) "yearly26Sets_1M.txt"
    it "works with two monthly issues" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_2M) "yearly26Sets_2M.txt"
    it "works with three monthly issues" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_3M) "yearly26Sets_3M.txt"
    it "works with one weekly issue (weekly-first)" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_1W) "yearly26Sets_1W.txt"
    it "works with two weekly issues (weekly, weekly-first)" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_2W) "yearly26Sets_2W.txt"
    it "works with three weekly issues (weekly, weekly-first, weekly-last)" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_3W) "yearly26Sets_3W.txt"
    it "works with one weekly & one monthly issue" $ do
        check_yearly (J.yearly26Sets 2019 testRefs_1W1M) "yearly26Sets_1W1M.txt"

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

-- =============================================================== --
-- Working with selection sets

testBV58N49 :: IO (T.Issue, [T.Citation])
-- ^Citations for Biochemistry (2019) Volume 58 Issue 49.
testBV58N49 = do
    let Just iss = J.lookupIssue TR.issueRefs "Biochemistry" (58,49)
    esummary <- Tx.readFile "tests/res/Biochemistry_V58N49_esummary.json"
    case P.parseCitations esummary of
         Right ([], cs) -> pure (iss, sortBy (comparing T.pmid) cs)
         Right (_, _  ) -> error $ "Test Error:"
                                   <> " res/Biochemistry_V58N49_esummary.json"
                                   <> " has missing citations!"
         _              -> error $ "Test Error:"
                                   <> " Cannot parse: "
                                   <> " res/Biochemistry_V58N49_esummary.json"

checkAddCitations :: (T.Content, [T.Citation]) -> [T.PMID] -> IO ()
checkAddCitations (T.Content sel cs, rest) allpmids = do
    let selected = T.selected sel
        added    = map T.pmid cs
        notAdded = map T.pmid rest
    added                    `shouldBe` selected
    sort (added <> notAdded) `shouldBe` allpmids

---------------------------------------------------------------------
-- Model.Journals.addCitations

spec_addCitations :: Spec
spec_addCitations = do
    it "works with no missing PMIDs"
        addCitationsNoMissingSpec

addCitationsNoMissingSpec :: IO ()
addCitationsNoMissingSpec = do
    (iss, cites) <- testBV58N49
    let pmids = sort . map T.pmid $ cites
        sel0  = []
        sel1  = [ "31710808" ]
        sel3  = [ "31710808", "31743022", "31746596" ]
        toSel = T.Selection iss
    checkAddCitations (J.addCitations (toSel sel0)  cites) pmids
    checkAddCitations (J.addCitations (toSel sel1)  cites) pmids
    checkAddCitations (J.addCitations (toSel sel3)  cites) pmids
    checkAddCitations (J.addCitations (toSel pmids) cites) pmids

---------------------------------------------------------------------
-- Model.Journals.missingPMIDs

spec_missingPMIDs :: Spec
spec_missingPMIDs = do
    it "(with addCitations) works with and without missing PMIDs"
        addCitationsMissingSpec

addCitationsMissingSpec :: IO ()
addCitationsMissingSpec = do
    (iss, cites) <- testBV58N49
    let pmids = sort . map T.pmid $ cites
        sel0  = [ "31710808", "31743022", "31746596" ]
        sel1  = [ "00000000", "31710808" ]
        sel2  = [ "00000000", "11111111" ]
        sel3  = [ "31710808", "00000000", "31743022", "31746596", "11111111" ]
        seln  = "00000000" : pmids <> [ "11111111" ]
        toSel = T.Selection iss
    (J.missingPMIDs . fst $ J.addCitations (toSel sel0) cites)
        `shouldBe` []
    (J.missingPMIDs . fst $ J.addCitations (toSel sel1) cites)
        `shouldBe` [ "00000000" ]
    (J.missingPMIDs . fst $ J.addCitations (toSel sel2) cites)
        `shouldBe` [ "00000000", "11111111" ]
    (J.missingPMIDs . fst $ J.addCitations (toSel sel3) cites)
        `shouldBe` [ "00000000", "11111111" ]
    (J.missingPMIDs . fst $ J.addCitations (toSel seln) cites)
        `shouldBe` [ "00000000", "11111111" ]

-- =============================================================== --
-- Working with rank matching

---------------------------------------------------------------------
-- Model.Journelas.score

spec_score :: Spec
spec_score = do
    let r1 = [ [1], [5,9,2], [3,11,4], [6], [7,13], [8] ]
        r2 = [ [1], [5], [9], [2], [3], [11], [4], [6], [7], [13], [8] ]
        e1 = [ (1,4), (2,3), (3,1), (4,1) ]
        e2 = [ (1,1), (2,1), (3,1), (4,1) ]
        e3 = [ (5,7), (9,7), (2,7), (3,6), (11,6), (6,2), (8,1) ]
        e4 = [ (5,9), (9,9), (2,9), (3,8), (11,8), (10,3), (12,3), (6,2), (8,1) ]
    it "works with no ranked papers" $ do
        J.score [1,2,3,4] [] `shouldBe` e2
    it "works with no indexed papers" $
        J.score [] r1 `shouldBe` []
    it "works with a single paper ranked that is not an indexed paper" $ do
        J.score [1,2,3,4] [[6]] `shouldBe` e2
    it "works with a single paper ranked that is an indexed paper" $ do
        J.score [1,2,3,4] [[3]] `shouldBe` [ (3,4), (1,1), (2,1), (4,1) ]
    it "works with a 4 indexed papers and only 3 are ranked (equally)" $ do
        J.score [1,2,3,4] [[3,4,1]] `shouldBe` [ (3,4), (4,4), (1,4), (2,1) ]
    it "works with 4 indexed papers and 11 singly ranked papers" $ do
        J.score [2,4,5,9] r2 `shouldBe` [ (5,4), (9,3), (2,2), (4,1) ]
    it "works with mixed rank-lists and different sets of indexed papers" $ do
        J.score [1,2,3,4]              r1 `shouldBe` e1
        J.score [2,5,9,3,11,6,8]       r1 `shouldBe` e3
        J.score [2,5,9,3,11,6,8,10,12] r1 `shouldBe` e4
