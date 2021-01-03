{-# LANGUAGE OverloadedStrings #-}

module View.Html
    ( tocsHtml
    , ranksHtml
    ) where

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Types as T
import qualified Model.Core.Core  as C
import qualified Model.Journals   as J
import qualified View.Help        as H
import qualified View.Templates   as Temp
import qualified View.Core        as Vc
import           Data.Text                ( Text           )
import           Data.Time                ( Day            )
import           View.Templates           ( fill, fillNone )

-- =============================================================== --
-- Helper functions

citationClass :: T.Citation -> Text
-- ^Generate a class name for a citation. If the issue is an extra
-- citation, then it gets the class name
--     extra-citations
-- Otherwise, it gets the class name of the issue.
citationClass c
    | T.isExtra c = "extra-citations"
    | otherwise   = issueClass c

issueClass :: T.HasIssue a => a -> Text
-- Generate the class name of an issue. The format is,
--     JournalName-Volume-Number
-- where the journal name has all spaces converted to underscores.
issueClass x = Tx.intercalate "-" fields
    where fields  = [ Vc.spaceToUnder . T.abbr . T.journal $ x
                    , C.tshow . T.volNo $ x
                    , C.tshow . T.issNo $ x
                    ]

classSelected :: [T.Selection] -> T.Citation -> Text
-- ^Determine the 'selected' class field for a citation's html.
classSelected sel c
    | isSelected = " class=\"selected\""
    | otherwise  = ""
    where isSelected = elem (T.pmid c) . J.pmidsInSelection $ sel

adminDefault :: Maybe Text -> Text
-- ^Handler for missing user names and emails.
adminDefault Nothing  = "the journal sets administrator"
adminDefault (Just x) = x

-- =============================================================== --
-- HTML component compositors for journal set tables of contents

tocsHtml :: Maybe Text -> Maybe Text -> Day
            -> T.JSet T.ToC -> T.Citations -> Text
-- ^Generate the complete html web document for a table of contents.
tocsHtml name email date jset@(T.JSet n tocs sel) cs =
    let dict = [ ( "meta",       tocsMeta n date                             )
               , ( "styles",     fillNone Temp.tocsCSS                       )
               , ( "script",     tocsScript jset                             )
               , ( "title",      "Journal Set " <> C.tshow n                 )
               , ( "instr",      fillNone Temp.tocsInstrHtml                 )
               , ( "tocs",       Tx.unlines . map (tocEntries cs sel) $ tocs )
               , ( "extra",      tocExtra cs jset                            )
               , ( "saveinstr",  tocsCreateSave name email                   )
               ]
    in fill (Map.fromList dict) Temp.tocsHtml

-- --------------------------------------------------------------- --
-- HTML meta data & fixed content for journal set tables of contents

tocsMeta :: Int -> Day -> Text
-- ^HTML meta data at the top of the ToC html document.
tocsMeta setNo date = fill (Map.fromList dict) Temp.tocsMetaHtml
    where dict = [ ( "title",   "Journal Set " <> C.tshow setNo )
                 , ( "date",    Vc.dateN date                   )
                 , ( "version", H.version                       )
                 ]

tocsCreateSave :: Maybe Text -> Maybe Text -> Text
-- ^HTML widgets for creating and saving the selection file.
tocsCreateSave name email = fill (Map.fromList dict) Temp.tocsCreateSaveHtml
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

issueHeaderAbbr :: T.HasIssue a => a -> Text
-- ^Construct html for the abbreviated header the given issue.
-- This is used for things like buttons.
issueHeaderAbbr iss = Tx.concat xs
    where xs = [ T.abbr . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               ]

tocEntries :: T.Citations -> [T.Selection] -> T.ToC -> Text
-- ^Construct html for all citations in an issue's Table of Contents.
-- If a url is specified, then assume there are missing citations.
tocEntries _  _   (T.ToC iss ""  []   ) = fill dict Temp.issueMissingHtml
    where dict  = Map.fromList [ ("issue", issueHeader iss )
                               ]
tocEntries cs sel (T.ToC iss ""  pmids) = fill dict Temp.issueHtml
    where cstxt = Tx.intercalate "\n" . map (tocEntry cs sel) $ pmids
          dict  = Map.fromList [ ("issue",     issueHeader iss)
                               , ("citations", cstxt          )
                               ]
tocEntries cs sel (T.ToC iss url pmids) = fill dict Temp.issueMissingLinkedHtml
    where cstxt = Tx.intercalate "\n" . map (tocEntry cs sel) $ pmids
          dict  = Map.fromList [ ("issue",     issueHeader iss    )
                               , ("citations", cstxt              )
                               , ("url",       "https://" <> url  )
                               , ("class",     issueClass iss     )
                               , ("abbr",      issueHeaderAbbr iss)
                               ]

tocExtra :: T.Citations -> T.JSet T.ToC -> Text
-- ^Construct html for all extra citations in journal set. Any
-- PMIDs that are not bound to an issue are placed in the extra
-- citations section. So, all citations are renderd as selected.
tocExtra cs (T.JSet _ _ sel) = fill dict Temp.tocsExtraCitationHtml
    where pmids = J.pmidsInSelectionFree sel
          cstxt = Tx.intercalate "\n" . map (tocEntry cs sel) $ pmids
          dict  = Map.fromList [ ("citations", cstxt ) ]

tocEntry :: T.Citations -> [T.Selection] -> T.PMID -> Text
-- ^Construct html for a single citation based on its PMID and a Map
-- of citations. If the PMID is not a member of the Citations map,
-- then it is ignored.
tocEntry cs sel pmid = maybe Tx.empty go . Map.lookup pmid $ cs
    where go c = fill (citationDict sel c) Temp.citationHtml

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
    where xys = Map.fromList [ ( "jsetHeader", Vc.jsetHeader jset )
                             , ( "savePrefix", savePrefix    jset )
                             , ( "issues",     issuesArray   jset )
                             ]

savePrefix :: T.HasIssue a => T.JSet a -> Text
-- ^Filename prefix for the selection text file.
savePrefix jset = "sel" <> C.tshow y <> "-" <> ( C.tshow . T.setNo $ jset )
    where y = T.year jset

issuesArray :: T.HasIssue a => T.JSet a -> Text
-- ^Constructs the JavaScript 'issues' array, which is used to track
-- each issue in the journal set.
issuesArray jset = Tx.intercalate ",\n" $ issueElements <> extrasElement
    where issueElements = map issuesArrayElement . T.issues $ jset
          extrasElement = [extrasArrayElement jset]

issuesArrayElement :: T.HasIssue a => a -> Text
-- ^Constructs each element of the JavaScrept 'issues' array. This is
-- used to track the specified issue in the journal set.
issuesArrayElement iss = fill dict Temp.tocsIssuesArrayJS
    where dict = Map.fromList [ ("class",  issueClass           iss )
                              , ("title",  (T.abbr . T.journal) iss )
                              , ("vol",    (C.tshow . T.volNo)  iss )
                              , ("number", (C.tshow . T.issNo)  iss )
                              , ("date",   (C.tshow . T.date )  iss )
                              ]

extrasArrayElement :: T.HasDate a => a -> Text
-- ^The JavaScrept 'issues' array element for extra citations, which
-- is used track extra articles not associated with configured issues.
extrasArrayElement x = fill dict Temp.tocsIssuesArrayJS
    where dict = Map.fromList [ ("class",  "extra-citations"     )
                              , ("title",  "Extra Articles"      )
                              , ("vol",    ""                    )
                              , ("number", ""                    )
                              , ("date",   (C.tshow . T.date ) x )
                              ]

-- =============================================================== --
-- HTML compositors for rank-lists to specify article preferences

ranksHtml :: Maybe Text -> Maybe Text -> Day -> T.Citations -> T.JSet a -> Text
-- ^Construct html for the rank-list of a journal set.
ranksHtml name email date cs (T.JSet n _ _ ) =
    let dict = [ ( "meta",      ranksMeta n date               )
               , ( "styles",    fillNone Temp.ranksCSS         )
               , ( "script",    fillNone Temp.ranksFunctionsJS )
               , ( "title",     "Journal Set " <> C.tshow n    )
               , ( "name",      adminDefault name              )
               , ( "email",     adminDefault  email            )
               , ( "citations", rankListContents cs            )
               ]
    in fill (Map.fromList dict) Temp.ranksHtml

---------------------------------------------------------------------
-- html meta data for rank-lists

ranksMeta :: Int -> Day -> Text
-- ^HTML meta data at the top of the rank-lists html document.
ranksMeta setNo date = fill (Map.fromList dict) Temp.ranksMetaHtml
    where dict = [ ( "title", "Journal Set " <> C.tshow setNo )
                 , ( "date",  Vc.dateN date                   )
                 , ("version", H.version                      )
                 ]

---------------------------------------------------------------------
-- html for the citations in a rank-list document

rankListContents :: T.Citations -> Text
-- ^Construct html for all rank list elements in the content list.
rankListContents =
    Tx.unlines . map (uncurry rankListElement) . zip [1..] . Map.elems

rankListElement :: Int -> T.Citation -> Text
-- ^Construct html for a citation element of a rank list.
rankListElement n c = fill (rankCitationDict n c) Temp.citationHtml

rankCitationDict :: Int -> T.Citation -> Map.Map Text Text
-- ^html template dictionary for rank list element.
rankCitationDict index c = Map.union m . citationDict [] $ c
    where m = Map.fromList [ ( "index", C.tshow index        )
                           , ( "length", Vc.citationLength c )
                           , ( "type",   "text"              )
                           , ( "class",  "_citation"         )
                           ]

-- =============================================================== --
-- html general template dictionaries for citations

citationDict :: [T.Selection] -> T.Citation -> Map.Map Text Text
-- ^Basic html template for a citation.
citationDict sel c = Map.fromList xys
    where xys = [ ("id",       T.pmid c                          )
                , ("selected", classSelected sel c               )
                , ("class",    citationClass     c               )
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
