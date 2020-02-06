{-# LANGUAGE OverloadedStrings #-}
module JournalsTests
    ( spec
    ) where

import qualified Data.Time             as Tm
import qualified Model.Core.Types      as T
import qualified Model.Core.References as R
import qualified Model.Journals        as J
import Test.Hspec                            ( Spec (..)
                                             , hspec
                                             , it
                                             , describe
                                             , shouldBe )

spec :: IO ()
spec = hspec $ do
    describe "Testing current issue (issueAtDate)" $ do
        earlyDates
        testScience
        testJacs
        testCellChemBiol

checkVolNum :: T.Issue -> Int -> Int -> IO ()
checkVolNum x v n = (T.volNo x, T.issNo x) `shouldBe` (v, n)

earlyDates :: Spec
earlyDates = do
    it "Handles early dates for Science correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2009 12 31) R.scienceRef
            n2 = J.issueAtDate (Tm.fromGregorian 2010  1  1) R.scienceRef
            n3 = J.issueAtDate (Tm.fromGregorian 2010  1  2) R.scienceRef
            n4 = J.issueAtDate (Tm.fromGregorian 2010  5  6) R.scienceRef
        checkVolNum n1 2010 5961
        checkVolNum n1 2010 5961
        checkVolNum n1 2010 5961
        checkVolNum n4 2010 5978
    it "Handles early dates for JACS correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2009 12 31) R.jacsRef
            n2 = J.issueAtDate (Tm.fromGregorian 2010  1 13) R.jacsRef
            n3 = J.issueAtDate (Tm.fromGregorian 2010  1 14) R.jacsRef
            n4 = J.issueAtDate (Tm.fromGregorian 2010  5  6) R.jacsRef
        checkVolNum n1 132 1
        checkVolNum n1 132 1
        checkVolNum n1 132 1
        checkVolNum n4 132 17
    it "Handles early dates for Cell Chem Biol correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2009 12 31) R.cellChemBiolRef
            n2 = J.issueAtDate (Tm.fromGregorian 2010  1  1) R.cellChemBiolRef
            n3 = J.issueAtDate (Tm.fromGregorian 2010  1  2) R.cellChemBiolRef
            n4 = J.issueAtDate (Tm.fromGregorian 2010  5  6) R.cellChemBiolRef
        checkVolNum n1 17 1
        checkVolNum n1 17 1
        checkVolNum n1 17 1
        checkVolNum n4 17 4

testScience :: Spec
testScience = do
    it "Identifies Science 2015-2016-2017 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2015 12 16) R.scienceRef
            n2  = J.issueAtDate (Tm.fromGregorian 2015 12 18) R.scienceRef
            n3  = J.issueAtDate (Tm.fromGregorian 2015 12 19) R.scienceRef
            n4  = J.issueAtDate (Tm.fromGregorian 2016 1   1) R.scienceRef
            n5  = J.issueAtDate (Tm.fromGregorian 2016 1   2) R.scienceRef
            n6  = J.issueAtDate (Tm.fromGregorian 2016 6  10) R.scienceRef
            n7  = J.issueAtDate (Tm.fromGregorian 2016 6  11) R.scienceRef
            n8  = J.issueAtDate (Tm.fromGregorian 2016 12 20) R.scienceRef
            n9  = J.issueAtDate (Tm.fromGregorian 2016 12 23) R.scienceRef
            n10 = J.issueAtDate (Tm.fromGregorian 2016 12 31) R.scienceRef
            n11 = J.issueAtDate (Tm.fromGregorian 2017 1   5) R.scienceRef
            n12 = J.issueAtDate (Tm.fromGregorian 2017 1   6) R.scienceRef
            n13 = J.issueAtDate (Tm.fromGregorian 2017 1   8) R.scienceRef
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
    it "Identifies Science 2018-2019-2020 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2018 12 18) R.scienceRef
            n2  = J.issueAtDate (Tm.fromGregorian 2018 12 21) R.scienceRef
            n3  = J.issueAtDate (Tm.fromGregorian 2019 1   4) R.scienceRef
            n4  = J.issueAtDate (Tm.fromGregorian 2019 1   5) R.scienceRef
            n5  = J.issueAtDate (Tm.fromGregorian 2019 6  13) R.scienceRef
            n6  = J.issueAtDate (Tm.fromGregorian 2019 6  14) R.scienceRef
            n7  = J.issueAtDate (Tm.fromGregorian 2019 6  15) R.scienceRef
            n8  = J.issueAtDate (Tm.fromGregorian 2019 12 19) R.scienceRef
            n9  = J.issueAtDate (Tm.fromGregorian 2019 12 20) R.scienceRef
            n10 = J.issueAtDate (Tm.fromGregorian 2019 12 31) R.scienceRef
            n11 = J.issueAtDate (Tm.fromGregorian 2020 1   2) R.scienceRef
            n12 = J.issueAtDate (Tm.fromGregorian 2020 1   3) R.scienceRef
            n13 = J.issueAtDate (Tm.fromGregorian 2020 1   6) R.scienceRef
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

testJacs :: Spec
-- ^Actual JACS publication dates are not always consistent and can
-- come either 6 or 8 days, but average out to 7. For example, the
-- last issue of 2013 was on December 26, but it should have been on
-- December 25. Tests assume JACS is always 7 days between issues.
-- So, the December 25 day is used in the tests.
testJacs = do
    it "Identifies JACS 2013-2014-2015 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2013 12 24) R.jacsRef
            n2  = J.issueAtDate (Tm.fromGregorian 2013 12 25) R.jacsRef
            n3  = J.issueAtDate (Tm.fromGregorian 2013 12 31) R.jacsRef
            n4  = J.issueAtDate (Tm.fromGregorian 2014 1   8) R.jacsRef
            n5  = J.issueAtDate (Tm.fromGregorian 2014 1   9) R.jacsRef
            n6  = J.issueAtDate (Tm.fromGregorian 2014 6  11) R.jacsRef
            n7  = J.issueAtDate (Tm.fromGregorian 2014 6  12) R.jacsRef
            n8  = J.issueAtDate (Tm.fromGregorian 2014 12 30) R.jacsRef
            n9  = J.issueAtDate (Tm.fromGregorian 2014 12 31) R.jacsRef
            n10 = J.issueAtDate (Tm.fromGregorian 2015 1   1) R.jacsRef
            n11 = J.issueAtDate (Tm.fromGregorian 2015 1  13) R.jacsRef
            n12 = J.issueAtDate (Tm.fromGregorian 2015 1  14) R.jacsRef
            n13 = J.issueAtDate (Tm.fromGregorian 2015 1  15) R.jacsRef
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
    it "Identifies JACS 2015-2016-2017 issues correctly" $ do
        let n1  = J.issueAtDate (Tm.fromGregorian 2015 12 29) R.jacsRef
            n2  = J.issueAtDate (Tm.fromGregorian 2015 12 30) R.jacsRef
            n3  = J.issueAtDate (Tm.fromGregorian 2015 12 31) R.jacsRef
            n4  = J.issueAtDate (Tm.fromGregorian 2016 1  13) R.jacsRef
            n5  = J.issueAtDate (Tm.fromGregorian 2016 1  14) R.jacsRef
            n6  = J.issueAtDate (Tm.fromGregorian 2016 6   8) R.jacsRef
            n7  = J.issueAtDate (Tm.fromGregorian 2016 6   9) R.jacsRef
            n8  = J.issueAtDate (Tm.fromGregorian 2016 12 27) R.jacsRef
            n9  = J.issueAtDate (Tm.fromGregorian 2016 12 28) R.jacsRef
            n10 = J.issueAtDate (Tm.fromGregorian 2016 12 30) R.jacsRef
            n11 = J.issueAtDate (Tm.fromGregorian 2017 1  10) R.jacsRef
            n12 = J.issueAtDate (Tm.fromGregorian 2017 1  11) R.jacsRef
            n13 = J.issueAtDate (Tm.fromGregorian 2017 1  12) R.jacsRef
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

testCellChemBiol :: Spec
testCellChemBiol = do
    it "Identifies Cell Chem Biol 2015-2016-2017 issues correctly" $ do
        let n1 = J.issueAtDate (Tm.fromGregorian 2015 12 16) R.cellChemBiolRef
            n2 = J.issueAtDate (Tm.fromGregorian 2016 1   1) R.cellChemBiolRef
            n3 = J.issueAtDate (Tm.fromGregorian 2016 2   1) R.cellChemBiolRef
            n4 = J.issueAtDate (Tm.fromGregorian 2016 6   1) R.cellChemBiolRef
            n5 = J.issueAtDate (Tm.fromGregorian 2016 6  11) R.cellChemBiolRef
            n6 = J.issueAtDate (Tm.fromGregorian 2016 12 23) R.cellChemBiolRef
            n7 = J.issueAtDate (Tm.fromGregorian 2016 12 31) R.cellChemBiolRef
            n8 = J.issueAtDate (Tm.fromGregorian 2017 1   5) R.cellChemBiolRef
            n9 = J.issueAtDate (Tm.fromGregorian 2017 2   1) R.cellChemBiolRef
        checkVolNum n1 22 11
        checkVolNum n2 22 12
        checkVolNum n3 23 1
        checkVolNum n4 23 5
        checkVolNum n5 23 5
        checkVolNum n6 23 11
        checkVolNum n7 23 11
        checkVolNum n8 23 12
        checkVolNum n9 24 1
