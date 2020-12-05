{-# LANGUAGE OverloadedStrings #-}

module View.Html
    ( tocsHtml
    , rankList
    ) where

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Types as T
import qualified Model.Core.Core  as C
import qualified View.Help        as H
import qualified View.Templates   as Temp
import qualified View.Core        as Vc
import           Data.Text                ( Text           )
import           Data.Time                ( Day            )
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

adminDefault :: Maybe Text -> Text
-- ^Handler for missing user names and emails.
adminDefault Nothing  = "the journal sets administrator"
adminDefault (Just x) = x

-- =============================================================== --
-- HTML component compositors for journal set tables of contents

tocsHtml :: Maybe Text -> Maybe Text -> Day
            -> T.JSet T.Content -> T.Citations -> Text
-- ^Generate the complete html web document for a table of contents.
tocsHtml name email date jset@(T.JSet n tocs sel) cs =
    let dict = [ ( "meta",       tocMeta n date                              )
               , ( "styles",     fillNone Temp.tocsCSS                       )
               , ( "script",     tocsScript jset                             )
               , ( "title",      "Journal Set " <> C.tshow n                 )
               , ( "instr",      fillNone Temp.tocInstrHtml                  )
               , ( "tocs",       Tx.unlines . map (tocEntries cs sel) $ tocs )
               , ( "saveinstr",  saveInstr name email                        )
               ]
    in fill (Map.fromList dict) Temp.tocsHtml

-- --------------------------------------------------------------- --
-- HTML meta data & fixed content for journal set tables of contents

tocMeta :: Int -> Day -> Text
-- ^HTML meta data at the top of the ToC html document.
tocMeta setNo date = fill (Map.fromList dict) Temp.tocMetaHtml
    where dict = [ ( "title",   "Journal Set " <> C.tshow setNo )
                 , ( "date",    Vc.dateN date                   )
                 , ( "version", H.version                       )
                 ]

saveInstr :: Maybe Text -> Maybe Text -> Text
saveInstr name email = fill (Map.fromList dict) Temp.tocSaveInstrHtml
    where dict = [ ( "name", adminDefault name   )
                 , ( "email", adminDefault email )
                 ]

-- --------------------------------------------------------------- --
-- Table of contents for each issue in the journal set

issueHeader :: T.HasIssue a => a -> Text
-- ^Construct html for the header of the ToC of the given issue.
issueHeader iss = Tx.concat xs
    where xs = [ T.name . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               ]

tocEntries :: T.Citations -> [T.PMID] -> T.Content -> Text
-- ^Construct html for all citations in a Table of Contents.
tocEntries _ _ (T.Content iss url [])
    | Tx.null url = fill xys Temp.issueMissingHtml
    | otherwise   = fill xys Temp.issueMissingLinkedHtml
    where xys = Map.fromList [ ("issue", issueHeader iss  )
                             , ("url",   "https://" <> url)
                             ]
tocEntries cs sel (T.Content iss _ pmids) = fill xys Temp.issueHtml
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

-- --------------------------------------------------------------- --
-- Javascript code for jset tables of contents html documents

tocsScript :: T.HasIssue a => T.JSet a -> Text
-- ^Generate javascript to run the html table of contents document
tocsScript jset = Tx.unlines [ tocsClasses, tocsGlobals jset, tocsFunctions ]

tocsClasses :: Text
-- ^Javascript classes necessary to run the html table of contents
tocsClasses = fillNone Temp.tocsClassesJS

tocsFunctions :: Text
-- ^Javascript functions necessary to run the html table of contents
tocsFunctions = fillNone Temp.tocsFunctionsJS

tocsGlobals :: T.HasIssue a => T.JSet a -> Text
-- ^Javascript global variables for the html table of contents.
tocsGlobals jset = fill xys Temp.tocsGlobalsJS
    where xys = Map.fromList [ ( "jsetHeader", Vc.jsetHeader jset            )
                             , ( "savePrefix", savePrefix jset               )
                             , ( "issues",     issuesArray . T.issues $ jset )
                             ]

savePrefix :: T.HasIssue a => T.JSet a -> Text
-- ^Filename prefix for the selection text file.
savePrefix jset = "sel" <> C.tshow y <> "-" <> ( C.tshow . T.setNo $ jset )
    where y = T.year jset

issuesArray :: T.HasIssue a => [a] -> Text
-- ^Constructs the JavaScript 'issues' array, which is used to track
-- each issue in the journal set.
issuesArray = Tx.intercalate ",\n" . map issuesArrayElement

issuesArrayElement :: T.HasIssue a => a -> Text
-- ^Constructs each element of the JavaScrept 'issues' array. This is
-- used to track the specified issue in the journal set.
issuesArrayElement iss = fill xys Temp.tocsIssuesArrayJS
    where xys = Map.fromList [ ("class",  className            iss )
                             , ("title",  (T.abbr . T.journal) iss )
                             , ("vol",    (C.tshow . T.volNo)  iss )
                             , ("number", (C.tshow . T.issNo)  iss )
                             , ("date",   (C.tshow . T.date )  iss )
                             ]

-- =============================================================== --
-- HTML compositors for rank-lists to specify article preferences

rankList :: Maybe Text -> Maybe Text -> T.Citations -> T.JSet a -> Text
-- ^Construct html for the rank list of a journal set.
rankList name email cs jset =
    let dict = [ ( "jsetTitle", "Journal Set " <> C.tshow (T.setNo jset) )
               , ( "name",      adminDefault name                        )
               , ( "email",     adminDefault  email                      )
               , ( "citations", rankListContents cs (T.selection jset)   )
               ]
    in fill (Map.fromList dict) Temp.rankListHtml

---------------------------------------------------------------------
-- html for the citations in a rank-list document

rankListContents :: T.Citations -> [T.PMID] -> Text
-- ^Construct html for all rank list elements in the content list.
rankListContents cs = Tx.unlines . map (uncurry rankListElement)
                                 . zip [1..]
                                 . map (flip Map.lookup cs)

rankListElement :: Int -> Maybe T.Citation -> Text
-- ^Construct html for a citation element of a rank list.
rankListElement _ Nothing  = Tx.empty
rankListElement n (Just c) = fill (rankCitationDict n c) Temp.citationHtml

rankCitationDict :: Int -> T.Citation -> Map.Map Text Text
-- ^html template dictionary for rank list element.
rankCitationDict index c = Map.union m . citationDict $ c
    where m = Map.fromList [ ( "index", C.tshow index        )
                           , ( "length", Vc.citationLength c )
                           , ( "type",   "text"              )
                           , ( "class",  "_citation"         )
                           ]

-- =============================================================== --
-- html general template dictionaries for citations

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
