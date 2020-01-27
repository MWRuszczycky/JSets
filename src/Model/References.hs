{-# LANGUAGE OverloadedStrings #-}

module Model.References
    ( issueRefs
    , acie
    , acieRef
    , biochemistry
    , biochemistryRef
    , cellChemBiol
    , cellChemBiolRef
    , jacs
    , jacsRef
    , nature
    , natureRef
    , natChem
    , natChemRef
    , science
    , scienceRef
    ) where

import qualified Data.Time       as Tm
import qualified Model.Types     as T

issueRefs :: [T.Issue]
issueRefs = [ cellChemBiolRef
            , natChemRef
            , jacsRef
            , scienceRef
            , biochemistryRef
            , natureRef
            ]

acie :: T.Journal
-- ^ACIE publication dates are irregular; however, there are always
-- 52 issues per at roughl 7 day intervals with approximately a 14
-- day interval at the new year.
acie = T.Journal {
      T.name   = "Angewandte Chemie International Edition"
    , T.abbr   = "ACIE"
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

biochemistry :: T.Journal
biochemistry = T.Journal {
      T.name   = "Biochemistry"
    , T.abbr   = "Biochemistry"
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

cellChemBiol :: T.Journal
cellChemBiol = T.Journal {
      T.name   = "Cell Chemical biology"
    , T.abbr   = "Cell Chem Biol"
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

jacs :: T.Journal
-- ^JACS issues are irregular, but follow the weekly-first pattern
-- on average, so that 6 day intervals are always balanced by 7 day
-- intervals. Thus, JACS is more predictable than ACIE.
jacs = T.Journal {
      T.name   = "Journal of the American Chemical Society"
    , T.abbr   = "JACS"
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

nature :: T.Journal
nature = T.Journal {
      T.name   = "Nature"
    , T.abbr   = "Nature"
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

natChem :: T.Journal
natChem = T.Journal {
      T.name   = "Nature Chemistry"
    , T.abbr   = "Nat Chem"
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

science :: T.Journal
science = T.Journal {
      T.name   = "Science"
    , T.abbr   = "Science"
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
