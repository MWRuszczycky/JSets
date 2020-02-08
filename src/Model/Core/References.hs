{-# LANGUAGE OverloadedStrings #-}

module Model.Core.References
    ( -- Utilities
      isAvailable
    , refIssue
    , issueRefs
    ) where

import qualified Data.Time        as Tm
import qualified Model.Core.Types as T
import           Data.Text              ( Text )
import           Data.List              ( find )

-- =============================================================== --
-- Utilities

isAvailable :: Text -> Bool
-- ^Determine whether a given journal has a reference issue.
isAvailable = maybe False (const True) . refIssue

refIssue :: Text -> Maybe T.Issue
-- ^Find a reference issue by its journal key.
refIssue key = find ( (== key) . T.key . T.journal ) issueRefs

issueRefs :: [T.Issue]
-- ^List of all available reference issues.
issueRefs = [ acieRef
            , biochemistryRef
            , cellChemBiolRef
            , jacsRef
            , natureRef
            , natChemRef
            , natChemBiolRef
            , pnasRef
            , scienceRef
            ]

-- =============================================================== --
-- Journals and associated reference issues

---------------------------------------------------------------------
-- Angewandte Chemie International Edition

acie :: T.Journal
-- ^ACIE publication dates are irregular; however, there are always
-- 52 issues per at roughl 7 day intervals with approximately a 14
-- day interval at the new year.
acie = T.Journal {
      T.name   = "Angewandte Chemie International Edition"
    , T.key    = "ACIE"
    , T.pubmed = "Angew Chem Int Ed Engl"
    , T.freq   = T.Weekly
    , T.resets = True
    }

acieRef :: T.Issue
acieRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 4
    , T.refNo   = 1
    , T.volNo   = 49
    , T.issNo   = 1
    , T.journal = acie
    }

---------------------------------------------------------------------
-- ACS Biochemistry

biochemistry :: T.Journal
biochemistry = T.Journal {
      T.name   = "Biochemistry"
    , T.key    = "Biochemistry"
    , T.pubmed = "Biochemistry"
    , T.freq   = T.WeeklyFirst
    , T.resets = True
    }

biochemistryRef :: T.Issue
biochemistryRef = T.Issue {
      T.date    = Tm.fromGregorian 2018 1 9
    , T.refNo   = 1
    , T.volNo   = 57
    , T.issNo   = 1
    , T.journal = biochemistry
    }

---------------------------------------------------------------------
-- Cell Chemical Biology

cellChemBiol :: T.Journal
cellChemBiol = T.Journal {
      T.name   = "Cell Chemical biology"
    , T.key    = "Cell Chem Biol"
    , T.pubmed = "Cell Chem Biol"
    , T.freq   = T.Monthly
    , T.resets = True
    }

cellChemBiolRef :: T.Issue
cellChemBiolRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 31
    , T.refNo   = 1
    , T.volNo   = 17
    , T.issNo   = 1
    , T.journal = cellChemBiol
    }

---------------------------------------------------------------------
-- ACS Journal of the American Chemical Society

jacs :: T.Journal
-- ^JACS issues are irregular, but follow the weekly-first pattern
-- on average, so that 6 day intervals are always balanced by 7 day
-- intervals. Thus, JACS is more predictable than ACIE.
jacs = T.Journal {
      T.name   = "Journal of the American Chemical Society"
    , T.key    = "JACS"
    , T.pubmed = "J Am Chem Soc"
    , T.freq   = T.WeeklyFirst
    , T.resets = True
    }

jacsRef :: T.Issue
jacsRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 13
    , T.refNo   = 1
    , T.volNo   = 132
    , T.issNo   = 1
    , T.journal = jacs
    }

---------------------------------------------------------------------
-- Nature

nature :: T.Journal
nature = T.Journal {
      T.name   = "Nature"
    , T.key    = "Nature"
    , T.pubmed = "Nature"
    , T.freq   = T.WeeklyLast
    , T.resets = False
    }

natureRef :: T.Issue
natureRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 7
    , T.refNo   = 1
    , T.volNo   = 2010
    , T.issNo   = 7277
    , T.journal = nature
    }

---------------------------------------------------------------------
-- Nature Chemistry

natChem :: T.Journal
natChem = T.Journal {
      T.name   = "Nature Chemistry"
    , T.key    = "Nat Chem"
    , T.pubmed = "Nat Chem"
    , T.freq   = T.Monthly
    , T.resets = True
    }

natChemRef :: T.Issue
natChemRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 1
    , T.refNo   = 1
    , T.volNo   = 2
    , T.issNo   = 1
    , T.journal = natChem
    }

---------------------------------------------------------------------
-- Nature Chemical Biology

natChemBiol :: T.Journal
natChemBiol = T.Journal {
      T.name   = "Nature Chemical Biology"
    , T.key    = "Nat Chem Biol"
    , T.pubmed = "Nat Chem Biol"
    , T.freq   = T.Monthly
    , T.resets = True
    }

natChemBiolRef :: T.Issue
natChemBiolRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 1
    , T.refNo   = 1
    , T.volNo   = 6
    , T.issNo   = 1
    , T.journal = natChemBiol
    }

---------------------------------------------------------------------
-- Proceedings of the National Academy of Sciences, U.S.A.

pnas :: T.Journal
pnas = T.Journal {
      T.name   = "Proceedings of the National Academy of Sciences U.S.A."
    , T.key    = "PNAS"
    , T.pubmed = "Proc Natl Acad Sci U S A"
    , T.freq   = T.Weekly
    , T.resets = True
    }

pnasRef :: T.Issue
pnasRef = T.Issue {
      T.date    = Tm.fromGregorian 2015 1 6
    , T.refNo   = 1
    , T.volNo   = 112
    , T.issNo   = 1
    , T.journal = pnas
    }

---------------------------------------------------------------------
-- Science

science :: T.Journal
science = T.Journal {
      T.name   = "Science"
    , T.key    = "Science"
    , T.pubmed = "Science"
    , T.freq   = T.WeeklyLast
    , T.resets = False
    }

scienceRef :: T.Issue
scienceRef = T.Issue {
      T.date    = Tm.fromGregorian 2010 1 1
    , T.refNo   = 1
    , T.volNo   = 2010
    , T.issNo   = 5961
    , T.journal = science
    }
