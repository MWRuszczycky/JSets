{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Html
    ( htmlToC
    --  htmlHeader
    --, htmlBody
    ) where

import qualified Data.Text             as Tx
import qualified Data.Map.Strict       as Map
import qualified Model.Core.Types      as T
import qualified Model.Core.Core       as C
import qualified Model.Journals        as J
import qualified Model.Core.Dates      as D
import qualified Model.Text.Templates  as Temp
import           Data.Text                      ( Text    )
import           Data.Char                      ( isSpace )
import           Model.Text.Templates           ( fill    )

-- =============================================================== --
-- Helper functions

className :: T.Issue -> Text
-- ^Generate a class name for a journal issue. The basic format is
-- _JournalName-Volume-Number
-- where the journal name has all spaces converted to underscores.
className iss = Tx.intercalate "-" xs
    where xs = [ ("_" <>) . spaceToUnder . T.key . T.journal $ iss
               , C.tshow . T.volNo $ iss
               , C.tshow . T.issNo $ iss
               ]

citationID :: T.Issue -> T.Citation -> Text
-- ^Generate an article citation for an article issue. The basic
-- format is: _JournalClassName-FirstPageNumber
citationID iss c = className iss <> "-" <> C.tshow p1
    where (p1,_) = T.pages c

spaceToUnder :: Text -> Text
-- ^Convert spaces to underscores.
spaceToUnder = Tx.map go
    where go x | isSpace x = '_'
               | otherwise = x

fixReserved :: Text -> Text
-- ^Convert html reserved characters to html compatible counterparts.
fixReserved = Tx.concatMap go
    where go '<' = "&lt"
          go '>' = "&gt"
          go '&' = "&amp"
          go x   = Tx.singleton x

-- =============================================================== --
-- Exported html document compositors

htmlToC :: Int -> [T.IssueToC] -> Text
-- ^Generate the complete html web document for a table of contents.
-- This webpage allows check-box selection of article citations and
-- autogeneration of the selection text file.
htmlToC setNumber tocs = fill "" (Map.fromList xys) Temp.tocsTemplate
    where jset = J.issueTocsToJSet setNumber tocs
          xys  = [ ( "jsetTitle",  "Journal Set " <> C.tshow setNumber )
                 , ( "jsetHeader", jsetHeader setNumber jset           )
                 , ( "savePrefix", savePrefix setNumber jset           )
                 , ( "issues",     issuesArray tocs                 )
                 , ( "tocs",       Tx.unlines . map issToCHtml $ tocs  )
                 ]

-- =============================================================== --
-- html component compositors

-- --------------------------------------------------------------- --
-- Javascript issue variables

issuesArray :: [T.IssueToC] -> Text
issuesArray = Tx.intercalate ",\n" . map (issueElement . T.tocIssue)

issueElement :: T.Issue -> Text
issueElement iss = fill "" (Map.fromList xys) Temp.issueTemplate
    where xys = [ ("class",  className           iss )
                , ("title",  (T.key . T.journal) iss )
                , ("vol",    (C.tshow . T.volNo) iss )
                , ("number", (C.tshow . T.issNo) iss )
                , ("date",   (C.tshow . T.date ) iss )
                ]

-- --------------------------------------------------------------- --
-- html for compositing the table of contents for a single issue

issToCHtml :: T.IssueToC -> Text
issToCHtml (T.IssueToC iss cs) = fill "" (Map.fromList xys) Temp.tocTemplate
    where msg = "<p>There are no articles listed for this issue at PubMed</p>"
          bdy | null cs   = Tx.replicate 12 " " <> msg
              | otherwise = Tx.intercalate "\n" . map ( citationHtml iss ) $ cs
          xys = [ ("issue",     issueHeader iss )
                , ("citations", bdy             )
                ]

issueHeader :: T.Issue -> Text
issueHeader iss = Tx.concat xs
    where xs = [ T.name . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               ]

citationHtml :: T.Issue -> T.Citation -> Text
citationHtml iss c = fill "" (Map.fromList xys) Temp.citationTemplate
    where (p0,pn) = T.pages c
          xys     = [ ("id",      citationID iss c                )
                    , ("class",   className iss                   )
                    , ("href",    T.doi c                         )
                    , ("title",   fixReserved . T.title $ c       )
                    , ("authors", fixReserved . T.authors $ c     )
                    , ("journal", T.name . T.journal $ iss        )
                    , ("volume",  C.tshow . T.volNo $ iss         )
                    , ("number",  C.tshow . T.issNo $ iss         )
                    , ("pages",   C.tshow p0 <> "-" <> C.tshow pn )
                    ]

-- --------------------------------------------------------------- --
-- Javascript elements for building the selection text file

jsetHeader :: Int -> T.JournalSet -> Text
-- ^Journal set header for generation of the selection text file.
jsetHeader setNumber jset = Tx.unwords xs
    where xs = [ C.tshow setNumber
               , "|"
               , C.tshow . J.dateOfJSet $ jset
               ]

savePrefix :: Int -> T.JournalSet -> Text
-- ^Filename prefix for the selection text file.
savePrefix setNumber jset = "sel" <> C.tshow y <> "-" <> C.tshow setNumber
    where y = D.getYear . J.dateOfJSet $ jset
