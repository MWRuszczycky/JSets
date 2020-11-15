{-# LANGUAGE OverloadedStrings #-}

module View.Html
    ( tocsHtml
    , rankList
    ) where

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Types as T
import qualified Model.Core.Core  as C
import qualified View.Templates   as Temp
import qualified View.Core        as Vc
import           Data.Text                ( Text           )
import           View.Templates           ( fill, fillNone )

-- =============================================================== --
-- Helper functions

className :: T.HasIssue a => a -> Text
-- ^Generate a class name for a journal issue. The basic format is
-- _JournalName-Volume-Number
-- where the journal name has all spaces converted to underscores.
className iss = Tx.intercalate "-" xs
    where xs = [ ("_" <>) . Vc.spaceToUnder . T.abbr . T.journal $ iss
               , C.tshow . T.volNo $ iss
               , C.tshow . T.issNo $ iss
               ]

-- =============================================================== --
-- Exported html document compositors

tocsHtml :: T.JSet T.Content -> T.Citations -> Text
-- ^Generate the complete html web document for a table of contents.
tocsHtml jset@(T.JSet n tocs sel) cs =
    let dict = [ ( "jsetTitle",  "Journal Set " <> C.tshow n                 )
               , ( "jsetHeader", Vc.jsetHeader jset                          )
               , ( "savePrefix", savePrefix jset                             )
               , ( "instr",      fillNone Temp.tocInstrHtml                  )
               , ( "saveinstr",  fillNone Temp.saveInstrHtml                 )
               , ( "issues",     issuesArray . T.issues $ jset               )
               , ( "tocs",       Tx.unlines . map (tocEntries cs sel) $ tocs )
               ]
    in fill (Map.fromList dict) Temp.tocsHtml

rankList :: Text -> Text -> T.Citations -> T.JSet a -> Text
-- ^Construct html for the rank list of a journal set.
rankList name email cs jset =
    let dict = [ ( "jsetTitle", "Journal Set " <> C.tshow (T.setNo jset) )
               , ( "name",      name                                     )
               , ( "email",     " at " <> email                          )
               , ( "citations", rankListContents cs (T.selection jset)   )
               ]
    in fill (Map.fromList dict) Temp.rankListHtml

-- =============================================================== --
-- html component compositors

-- --------------------------------------------------------------- --
-- Javascript and issue components

issuesArray :: T.HasIssue a => [a] -> Text
-- ^Constructs the JavaScript 'issues' array, which is used to track
-- each issue in the journal set.
issuesArray = Tx.intercalate ",\n" . map issueElement

issueElement :: T.HasIssue a => a -> Text
-- ^Constructs each element of the JavaScrept 'issues' array. This is
-- used to track the specified issue in the journal set.
issueElement iss = fill xys Temp.issueJS
    where xys = Map.fromList [ ("class",  className            iss )
                             , ("title",  (T.abbr . T.journal) iss )
                             , ("vol",    (C.tshow . T.volNo)  iss )
                             , ("number", (C.tshow . T.issNo)  iss )
                             , ("date",   (C.tshow . T.date )  iss )
                             ]

issueHeader :: T.HasIssue a => a -> Text
-- ^Construct html for the header of the ToC of the given issue.
issueHeader iss = Tx.concat xs
    where xs = [ T.name . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               ]

-- --------------------------------------------------------------- --
-- html of the citations in the Tables of Contents or Rank lists

tocEntries :: T.Citations -> [T.PMID] -> T.Content -> Text
-- ^Construct html for all citations in a Table of Contents.
tocEntries _ _ (T.Content iss url [])
    | Tx.null url = fill xys Temp.tocMissingHtml
    | otherwise   = fill xys Temp.tocMissingUrlHtml
    where xys = Map.fromList [ ("issue", issueHeader iss  )
                             , ("url",   "https://" <> url)
                             ]
tocEntries cs sel (T.Content iss _ pmids) = fill xys Temp.tocHtml
    where cstxt = Tx.intercalate "\n" . map (tocEntry cs sel) $ pmids
          xys   = Map.fromList [ ("issue",     issueHeader iss)
                               , ("citations", cstxt          )
                               ]

tocEntry :: T.Citations -> [T.PMID] -> T.PMID -> Text
-- ^Construct html for a single citation based on its PMID and a Map
-- of citations. If the PMID is not a member of the Citations map,
-- then it is ignored.
tocEntry cs sel pmid = maybe Tx.empty go . Map.lookup pmid $ cs
    where go c | elem pmid sel = fill (citationDictSel c) Temp.citationHtml
               | otherwise     = fill (citationDict    c) Temp.citationHtml

---------------------------------------------------------------------
-- html for tables of contents with only selected citations

-- rankListContents :: [T.Content] -> Text
rankListContents :: T.Citations -> [T.PMID] -> Text

-- ^Construct html for all rank list elements in the content list.
rankListContents cs = Tx.unlines . map (uncurry rankListElement)
                                 . zip [1..]
                                 . map (flip Map.lookup cs)

rankListElement :: Int -> Maybe T.Citation -> Text
-- ^Construct html for a citation element of a rank list.
rankListElement _ Nothing  = Tx.empty
rankListElement n (Just c) = fill (rankCitationDict n c) Temp.citationHtml

---------------------------------------------------------------------
-- html template dictionaries for citations

rankCitationDict :: Int -> T.Citation -> Map.Map Text Text
-- ^html template dictionary for rank list element.
rankCitationDict index c = Map.union m . citationDict $ c
    where m = Map.fromList [ ( "index", C.tshow index        )
                           , ( "length", Vc.citationLength c )
                           , ( "type",   "text"              )
                           , ( "class",  "_citation"         )
                           ]

citationDictSel :: T.Citation -> Map.Map Text Text
-- ^Basic html template for a selected citation.
citationDictSel = Map.insert "selected" " class=\"selected\"" . citationDict

citationDict :: T.Citation -> Map.Map Text Text
-- ^Basic html template for a citation.
citationDict c = Map.fromList xys
    where xys = [ ("id",       T.pmid c                          )
                , ("class",    className . T.issue $ c           )
                , ("type",     "checkbox"                        )
                , ("href",     T.doi c                           )
                , ("title",    T.title c                         )
                , ("authors",  Vc.authorLine c                   )
                , ("journal",  T.name  . T.journal . T.issue $ c )
                , ("volume",   C.tshow . T.volNo   . T.issue $ c )
                , ("number",   C.tshow . T.issNo   . T.issue $ c )
                , ("pages",    C.tshow . T.pages   $ c           )
                , ("pmid",     T.pmid c                          )
                ]

-- --------------------------------------------------------------- --
-- Javascript elements for building the selection text file

savePrefix :: T.HasIssue a => T.JSet a -> Text
-- ^Filename prefix for the selection text file.
savePrefix jset = "sel" <> C.tshow y <> "-" <> ( C.tshow . T.setNo $ jset )
    where y = T.year jset
