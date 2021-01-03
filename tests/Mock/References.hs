{-# LANGUAGE OverloadedStrings #-}

module Mock.References
    ( refIssue
    , issueRefs
    , acieRef
    , biochemistryRef
    , cellChemBiolEarlyRef
    , cellChemBiolEndRef
    , cellChemBiolMidRef
    , jacsRef
    , natureRef
    , natChemRef
    , natChemBiolRef
    , pnasRef
    , scienceRef
    , orgLettRef
    ) where

import qualified Data.Time        as Tm
import qualified Model.Core.Types as T
import           Data.Text              ( Text )
import           Data.List              ( find )

-- =============================================================== --
-- Accessors

refIssue :: Text -> Maybe T.Issue
-- ^Find a reference issue by its journal abbreviation.
refIssue abbr = find ( (== abbr) . T.abbr . T.journal ) issueRefs

issueRefs :: [T.Issue]
-- ^List of all available reference issues.
issueRefs = [ acieRef
            , biochemistryRef
            , cellChemBiolEarlyRef
            , cellChemBiolEndRef
            , cellChemBiolMidRef
            , jacsRef
            , natureRef
            , natChemRef
            , natChemBiolRef
            , pnasRef
            , scienceRef
            , orgLettRef
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
      T.name     = "Angewandte Chemie International Edition"
    , T.abbr     = "ACIE"
    , T.pubmed   = "Angew Chem Int Ed Engl"
    , T.freq     = T.EveryNWeeks 1
    , T.resets   = True
    , T.mincount = 20
    , T.followed = True
    }

acieRef :: T.Issue
acieRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 4
    , T.theVolNo   = 49
    , T.theIssNo   = 1
    , T.theJournal = acie
    }

---------------------------------------------------------------------
-- ACS Biochemistry

biochemistry :: T.Journal
biochemistry = T.Journal {
      T.name     = "Biochemistry"
    , T.abbr     = "Biochemistry"
    , T.pubmed   = "Biochemistry"
    , T.freq     = T.WeeklyFirst
    , T.resets   = True
    , T.mincount = 3
    , T.followed = True
    }

biochemistryRef :: T.Issue
biochemistryRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2018 1 9
    , T.theVolNo   = 57
    , T.theIssNo   = 1
    , T.theJournal = biochemistry
    }

---------------------------------------------------------------------
-- Cell Chemical Biology

cellChemBiolEarly :: T.Journal
cellChemBiolEarly = T.Journal {
      T.name     = "Cell Chemical Biology"
    , T.abbr     = "Cell Chem Biol"
    , T.pubmed   = "Cell Chem Biol"
    , T.freq     = T.Monthly
    , T.resets   = True
    , T.mincount = 10
    , T.followed = True
    }

cellChemBiolEarlyRef :: T.Issue
cellChemBiolEarlyRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2017 1 19
    , T.theVolNo   = 24
    , T.theIssNo   = 1
    , T.theJournal = cellChemBiolEarly
    }


cellChemBiolEnd :: T.Journal
cellChemBiolEnd = T.Journal {
      T.name     = "Cell Chemical Biology"
    , T.abbr     = "Cell Chem Biol"
    , T.pubmed   = "Cell Chem Biol"
    , T.freq     = T.EndMonthly
    , T.resets   = True
    , T.mincount = 10
    , T.followed = True
    }

cellChemBiolEndRef :: T.Issue
cellChemBiolEndRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 31
    , T.theVolNo   = 17
    , T.theIssNo   = 1
    , T.theJournal = cellChemBiolEnd
    }

cellChemBiolMid :: T.Journal
cellChemBiolMid = T.Journal {
      T.name     = "Cell Chemical Biology"
    , T.abbr     = "Cell Chem Biol Rel"
    , T.pubmed   = "Cell Chem Biol Rel"
    , T.freq     = T.MidMonthly
    , T.resets   = True
    , T.mincount = 10
    , T.followed = True
    }

cellChemBiolMidRef :: T.Issue
cellChemBiolMidRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2015 1 22
    , T.theVolNo   = 22
    , T.theIssNo   = 1
    , T.theJournal = cellChemBiolMid
    }

---------------------------------------------------------------------
-- ACS Journal of the American Chemical Society

jacs :: T.Journal
-- ^JACS issues are irregular, but follow the weekly-first pattern
-- on average, so that 6 day intervals are always balanced by 7 day
-- intervals. Thus, JACS is more predictable than ACIE.
jacs = T.Journal {
      T.name     = "Journal of the American Chemical Society"
    , T.abbr     = "JACS"
    , T.pubmed   = "J Am Chem Soc"
    , T.freq     = T.WeeklyFirst
    , T.resets   = True
    , T.mincount = 20
    , T.followed = True
    }

jacsRef :: T.Issue
jacsRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 13
    , T.theVolNo   = 132
    , T.theIssNo   = 1
    , T.theJournal = jacs
    }

---------------------------------------------------------------------
-- Nature

nature :: T.Journal
nature = T.Journal {
      T.name     = "Nature"
    , T.abbr     = "Nature"
    , T.pubmed   = "Nature"
    , T.freq     = T.WeeklyLast
    , T.resets   = False
    , T.mincount = 10
    , T.followed = True
    }

natureRef :: T.Issue
natureRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 7
    , T.theVolNo   = 2010
    , T.theIssNo   = 7277
    , T.theJournal = nature
    }

---------------------------------------------------------------------
-- Nature Chemistry

natChem :: T.Journal
natChem = T.Journal {
      T.name     = "Nature Chemistry"
    , T.abbr     = "Nat Chem"
    , T.pubmed   = "Nat Chem"
    , T.freq     = T.EndMonthly
    , T.resets   = True
    , T.mincount = 10
    , T.followed = True
    }

natChemRef :: T.Issue
natChemRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 1
    , T.theVolNo   = 2
    , T.theIssNo   = 1
    , T.theJournal = natChem
    }

---------------------------------------------------------------------
-- Nature Chemical Biology

natChemBiol :: T.Journal
natChemBiol = T.Journal {
      T.name     = "Nature Chemical Biology"
    , T.abbr     = "Nat Chem Biol"
    , T.pubmed   = "Nat Chem Biol"
    , T.freq     = T.EndMonthly
    , T.resets   = True
    , T.mincount = 10
    , T.followed = True
    }

natChemBiolRef :: T.Issue
natChemBiolRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 1
    , T.theVolNo   = 6
    , T.theIssNo   = 1
    , T.theJournal = natChemBiol
    }

---------------------------------------------------------------------
-- Proceedings of the National Academy of Sciences, U.S.A.

pnas :: T.Journal
pnas = T.Journal {
      T.name     = "Proceedings of the National Academy of Sciences U.S.A."
    , T.abbr     = "PNAS"
    , T.pubmed   = "Proc Natl Acad Sci U S A"
    , T.freq     = T.EveryNWeeks 1
    , T.resets   = True
    , T.mincount = 20
    , T.followed = True
    }

pnasRef :: T.Issue
pnasRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2015 1 6
    , T.theVolNo   = 112
    , T.theIssNo   = 1
    , T.theJournal = pnas
    }

---------------------------------------------------------------------
-- Science

science :: T.Journal
science = T.Journal {
      T.name     = "Science"
    , T.abbr     = "Science"
    , T.pubmed   = "Science"
    , T.freq     = T.WeeklyLast
    , T.resets   = False
    , T.mincount = 10
    , T.followed = True
    }

scienceRef :: T.Issue
scienceRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2010 1 1
    , T.theVolNo   = 2010
    , T.theIssNo   = 5961
    , T.theJournal = science
    }

---------------------------------------------------------------------
-- Organic Letters

orgLett :: T.Journal
orgLett = T.Journal {
      T.name     = "Organic Letters"
    , T.abbr     = "Org Lett"
    , T.pubmed   = "Org Lett"
    , T.freq     = T.SemiMonthly
    , T.resets   = True
    , T.mincount = 20
    , T.followed = True
    }

orgLettRef :: T.Issue
orgLettRef = T.Issue {
      T.theDate    = Tm.fromGregorian 2017 1 6
    , T.theVolNo   = 19
    , T.theIssNo   = 1
    , T.theJournal = orgLett
    }
