{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Html
    ( htmlToCPropose
    , htmlToCSelect
    , htmlToCRank
    ) where

import qualified Data.Text             as Tx
import qualified Data.Map.Strict       as Map
import qualified Model.Core.Types      as T
import qualified Model.Core.Core       as C
import qualified Model.Journals        as J
import qualified Model.Core.Dates      as D
import qualified Model.Text.Templates  as Temp
import           Data.Text                      ( Text           )
import           Data.Char                      ( isSpace        )
import           Model.Text.Templates           ( fill, fillNone )

-- =============================================================== --
-- Helper functions

className :: T.HasIssue a => a -> Text
-- ^Generate a class name for a journal issue. The basic format is
-- _JournalName-Volume-Number
-- where the journal name has all spaces converted to underscores.
className iss = Tx.intercalate "-" xs
    where xs = [ ("_" <>) . spaceToUnder . T.abbr . T.journal $ iss
               , C.tshow . T.volNo $ iss
               , C.tshow . T.issNo $ iss
               ]

citationID :: T.HasIssue a => a -> T.Citation -> Text
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

htmlToCPropose :: T.JournalSet T.IssueContent -> Text
-- ^Generate the complete html web document for a table of contents.
-- This webpage allows check-box selection of article citations and
-- autogeneration of the selection text file.
-- The 'propose' style is used to select citations for consideration.
htmlToCPropose jset = fill (Map.fromList xys) Temp.tocsTemplate
    where xys = [ ( "jsetTitle",  "Journal Set " <> C.tshow (T.setNo jset) )
                , ( "jsetHeader", jsetHeader jset                          )
                , ( "savePrefix", savePrefix jset                          )
                , ( "instr",      fillNone Temp.instrCTemplate             )
                , ( "issues",     issuesArray . T.issues $ jset            )
                , ( "tocs",       allCitationsHtml . T.issues $ jset       )
                ]

htmlToCSelect :: T.JournalSet T.IssueContent -> Text
-- ^Generate the complete html web document for a table of contents.
-- This webpage allows check-box selection of article citations and
-- autogeneration of the selection text file.
-- The 'select' style is used for selecting citations for review.
htmlToCSelect jset = fill (Map.fromList xys) Temp.tocsTemplate
    where xys = [ ( "jsetTitle",  "Journal Set " <> C.tshow (T.setNo jset) )
                , ( "jsetHeader", jsetHeader jset                          )
                , ( "savePrefix", savePrefix jset                          )
                , ( "instr",      fillNone Temp.instrRTemplate             )
                , ( "issues",     issuesArray . T.issues $ jset            )
                , ( "tocs",       allCitationsHtml . T.issues $ jset       )
                ]

htmlToCRank :: T.JournalSet T.IssueContent -> Text
htmlToCRank jset = fill (Map.fromList xys) Temp.ranksTemplate
    where iss = J.selectContent . T.issues $ jset
          xys = [ ( "jsetTitle", "Journal Set " <> C.tshow (T.setNo jset) )
                , ( "citations", onlySelectedHtml iss                     )
                ]

-- =============================================================== --
-- html component compositors

-- --------------------------------------------------------------- --
-- Javascript issue variables

issuesArray :: T.HasIssue a => [a] -> Text
issuesArray = Tx.intercalate ",\n" . map issueElement

issueElement :: T.HasIssue a => a -> Text
issueElement iss = fill xys Temp.issueTemplate
    where xys = Map.fromList [ ("class",  className            iss )
                             , ("title",  (T.abbr . T.journal) iss )
                             , ("vol",    (C.tshow . T.volNo)  iss )
                             , ("number", (C.tshow . T.issNo)  iss )
                             , ("date",   (C.tshow . T.date )  iss )
                             ]

-- --------------------------------------------------------------- --
-- html for the tables of contents with all citations

allCitationsHtml :: [T.IssueContent] -> Text
allCitationsHtml = Tx.unlines . map allCitations

allCitations :: T.IssueContent -> Text
-- ^Display all citations in the issue, and provide a note if none.
allCitations (T.IssueContent sel cs) =
    let msg = "<p>There are no article listed for this issue at PubMed</p>"
        bdy | null cs   = Tx.replicate 12 " " <> msg
            | otherwise = Tx.intercalate "\n" . map (citationHtml sel) $ cs
        xys = Map.fromList [ ("issue", issueHeader sel), ("citations", bdy) ]
    in  fill xys Temp.tocTemplate

citationHtml :: T.Selection -> T.Citation -> Text
citationHtml sel@(T.Selection _ ps) c
    | elem p0 ps = fill ( go " class=\"selected\"" ) Temp.citationTemplate
    | otherwise  = fill ( go Tx.empty              ) Temp.citationTemplate
    where p0   = fst . T.pages $ c
          go u = Map.insert "selected" u . citationDict sel $ c

---------------------------------------------------------------------
-- html for tables of contents with only selected citations

onlySelectedHtml :: [T.IssueContent] -> Text
-- ^Dispaly only selected citations, and nothing if none selected.
onlySelectedHtml toc = Tx.unlines . zipWith go [1..] $ ds
    where ds     = concatMap selectedDict toc
          go k d = fill ( Map.insert "index" (C.tshow k) d )
                        Temp.citationTemplate

citationLength :: T.Citation -> Text
citationLength c
    | diff < 0    = " (online)"
    | dn - d0 < 6 = " (short)"
    | otherwise   = " (long)"
    where (T.PageNumber _ d0, T.PageNumber _ dn) = T.pages c
          diff = dn - d0

selectedDict :: T.IssueContent -> [Map.Map Text Text]
selectedDict (T.IssueContent sel cs) = map go cs
    where go c = Map.union ( ud c ) . citationDict sel $ c
          ud c = Map.fromList [ ( "length", citationLength c )
                              , ( "type",   "text")
                              , ( "class",  "_citation"      )
                              ]

---------------------------------------------------------------------
-- html for all tables of contents

issueHeader :: T.HasIssue a => a -> Text
issueHeader iss = Tx.concat xs
    where xs = [ T.name . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               ]

citationDict :: T.Selection -> T.Citation -> Map.Map Text Text
citationDict (T.Selection iss _) c = Map.fromList xys
    where (p0,pn) = T.pages c
          xys     = [ ("id",       citationID iss c                )
                    , ("class",    className iss                   )
                    , ("type",     "checkbox"                      )
                    , ("href",     T.doi c                         )
                    , ("title",    fixReserved . T.title $ c       )
                    , ("authors",  fixReserved . T.authors $ c     )
                    , ("journal",  T.name . T.journal $ iss        )
                    , ("volume",   C.tshow . T.volNo $ iss         )
                    , ("number",   C.tshow . T.issNo $ iss         )
                    , ("pages",    C.tshow p0 <> "-" <> C.tshow pn )
                    ]

-- --------------------------------------------------------------- --
-- Javascript elements for building the selection text file

jsetHeader :: T.HasIssue a => T.JournalSet a -> Text
-- ^Journal set header for generation of the selection text file.
jsetHeader jset = Tx.unwords xs
    where xs = [ C.tshow . T.setNo $ jset
               , "|"
               , C.tshow . J.dateOfJSet $ jset
               ]

savePrefix :: T.HasIssue a => T.JournalSet a -> Text
-- ^Filename prefix for the selection text file.
savePrefix jset = "sel" <> C.tshow y <> "-" <> ( C.tshow . T.setNo $ jset )
    where y = D.getYear . J.dateOfJSet $ jset
