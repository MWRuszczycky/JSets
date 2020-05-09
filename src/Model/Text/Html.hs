{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Html
    ( htmlToC
    ) where

import qualified Data.Text             as Tx
import qualified Data.Map.Strict       as Map
import qualified Model.Core.Types      as T
import qualified Model.Core.Core       as C
import qualified Model.Journals        as J
import qualified Model.Core.Dates      as D
import qualified Model.Text.Templates  as Temp
import           Data.Text                      ( Text      )
import           Data.Char                      ( isSpace   )
import           Model.Text.Templates           ( fill      )

-- =============================================================== --
-- Helper functions

className :: T.IsIssue a => a -> Text
-- ^Generate a class name for a journal issue. The basic format is
-- _JournalName-Volume-Number
-- where the journal name has all spaces converted to underscores.
className iss = Tx.intercalate "-" xs
    where xs = [ ("_" <>) . spaceToUnder . T.abbr . T.journal $ iss
               , C.tshow . T.volNo $ iss
               , C.tshow . T.issNo $ iss
               ]

citationID :: T.IsIssue a => a -> T.Citation -> Text
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

htmlToC :: T.JournalSet T.CitedIssue -> Text -> Text
-- ^Generate the complete html web document for a table of contents.
-- This webpage allows check-box selection of article citations and
-- autogeneration of the selection text file.
htmlToC jset instr = fill (Map.fromList xys) Temp.tocsTemplate
    where xys = [ ( "jsetTitle",  "Journal Set " <> C.tshow (T.setNo jset)     )
                , ( "jsetHeader", jsetHeader jset                              )
                , ( "savePrefix", savePrefix jset                              )
                , ( "instr",      instr                                        )
                , ( "issues",     issuesArray . T.issues $ jset                )
                , ( "tocs",       Tx.unlines . map issueHtml . T.issues $ jset )
                ]

-- =============================================================== --
-- html component compositors

-- --------------------------------------------------------------- --
-- Javascript issue variables

issuesArray :: T.IsIssue a => [a] -> Text
issuesArray = Tx.intercalate ",\n" . map issueElement

issueElement :: T.IsIssue a => a -> Text
issueElement iss = fill xys Temp.issueTemplate
    where xys = Map.fromList [ ("class",  className            iss )
                             , ("title",  (T.abbr . T.journal) iss )
                             , ("vol",    (C.tshow . T.volNo)  iss )
                             , ("number", (C.tshow . T.issNo)  iss )
                             , ("date",   (C.tshow . T.date )  iss )
                             ]

-- --------------------------------------------------------------- --
-- html for compositing the table of contents for a single issue

issueHtml :: T.CitedIssue -> Text
issueHtml (T.CitedIssue selIss cs) =
    let msg = "<p>There are no article listed for this issue at PubMed</p>"
        bdy | null cs   = Tx.replicate 12 " " <> msg
            | otherwise = Tx.intercalate "\n" . map (citationHtml selIss) $ cs
        xys = Map.fromList [ ("issue", issueHeader selIss), ("citations", bdy) ]
    in  fill xys Temp.tocTemplate

issueHeader :: T.IsIssue a => a -> Text
issueHeader iss = Tx.concat xs
    where xs = [ T.name . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               ]

citationHtml :: T.SelIssue -> T.Citation -> Text
citationHtml (T.SelIssue iss sel) c
    | elem p0 sel = fill ( xys " class=\"selected\"" ) Temp.citationTemplate
    | otherwise   = fill ( xys Tx.empty              ) Temp.citationTemplate
    where (p0,pn) = T.pages c
          xys u = Map.fromList [ ("selected", u                               )
                               , ("id",       citationID iss c                )
                               , ("class",    className iss                   )
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

jsetHeader :: T.IsIssue a => T.JournalSet a -> Text
-- ^Journal set header for generation of the selection text file.
jsetHeader jset = Tx.unwords xs
    where xs = [ C.tshow . T.setNo $ jset
               , "|"
               , C.tshow . J.dateOfJSet $ jset
               ]

savePrefix :: T.IsIssue a => T.JournalSet a -> Text
-- ^Filename prefix for the selection text file.
savePrefix jset = "sel" <> C.tshow y <> "-" <> ( C.tshow . T.setNo $ jset )
    where y = D.getYear . J.dateOfJSet $ jset
