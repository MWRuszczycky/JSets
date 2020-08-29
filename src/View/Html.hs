{-# LANGUAGE OverloadedStrings #-}

module View.Html
    ( tocBasic
    , tocInstr
    , rankList
    ) where

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Types as T
import qualified Model.Core.Core  as C
import qualified View.Templates   as Temp
import qualified View.Core        as Vc
import           Data.List                ( foldl'         )
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

tocBasic :: T.JSet T.Content -> Text
-- ^Generate the complete html web document for a table of contents.
-- This is the most simple output that contains no instructions or
-- JavaScript. It is basically equivalent to the Markdown output;
-- however, selected citations are highlighted.
tocBasic jset@(T.JSet n contents) =
    let dict = [ ( "jsetTitle",  "Journal Set " <> C.tshow n            )
               , ( "jsetHeader", Vc.jsetHeader jset                     )
               , ( "savePrefix", savePrefix jset                        )
               , ( "saveinstr",  fillNone Temp.saveInstrBasicHtml       )
               , ( "issues",     issuesArray . T.issues $ jset          )
               , ( "tocs",       Tx.unlines . map tocEntries $ contents )
               ]
    in fill (Map.fromList dict) Temp.tocsHtml

tocInstr :: Text -> Text -> T.JSet T.Content -> Text
-- ^Generate the complete html web document for a table of contents.
-- This webpage allows check-box selection of article citations and
-- autogeneration of the selection text file. Include instructions at
-- the top for how to use the checkboxes.
tocInstr name email jset@(T.JSet n contents) =
    let dict = [ ( "jsetTitle",  "Journal Set " <> C.tshow n            )
               , ( "jsetHeader", Vc.jsetHeader jset                     )
               , ( "savePrefix", savePrefix jset                        )
               , ( "instr",      fillNone Temp.tocInstrHtml             )
               , ( "saveinstr",  saveInstructions name email            )
               , ( "issues",     issuesArray . T.issues $ jset          )
               , ( "tocs",       Tx.unlines . map tocEntries $ contents )
               ]
    in fill (Map.fromList dict) Temp.tocsHtml

rankList :: Text -> Text -> T.JSet T.Content -> Text
-- ^Construct html for the rank list of a journal set. Each Content
-- in the set must contain only the citations that have been selected.
rankList name email (T.JSet n contents) =
    let dict = [ ( "jsetTitle", "Journal Set " <> C.tshow n )
               , ( "name",      name                        )
               , ( "email",     " at " <> email             )
               , ( "citations", rankListContents contents   )
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

---------------------------------------------------------------------
-- Other html components

saveInstructions :: Text -> Text -> Text
-- ^Construct html for the save instructions for select and propose
-- style html tables of contents.
saveInstructions name email = fill (Map.fromList dict) Temp.saveInstrHtml
    where dict = [ ( "name",  name            )
                 , ( "email", " at " <> email )
                 ]

-- --------------------------------------------------------------- --
-- html of the citations in the Tables of Contents or Rank lists

tocEntries :: T.Content -> Text
-- ^Construct html for all citations in a Table of Contents.
tocEntries (T.Content sel url [])
    | Tx.null url = fill xys Temp.tocMissingHtml
    | otherwise   = fill xys Temp.tocMissingUrlHtml
    where xys = Map.fromList [ ("issue", issueHeader sel  )
                             , ("url",   "https://" <> url)
                             ]
tocEntries (T.Content sel _ cs) = fill xys Temp.tocHtml
    where cstxt = Tx.intercalate "\n" . map (tocEntry sel) $ cs
          xys   = Map.fromList [ ("issue",     issueHeader sel)
                               , ("citations", cstxt          )
                               ]

tocEntry :: T.Selection -> T.Citation -> Text
-- ^Construct html for a single citation in a Table of Contents.
-- Highlight citation if it has been selected.
tocEntry sel@(T.Selection _ ps) c
    | elem p ps = fill ( go " class=\"selected\"" ) Temp.citationHtml
    | otherwise = fill ( go Tx.empty              ) Temp.citationHtml
    where p    = T.pmid c
          go u = Map.insert "selected" u . citationDict sel $ c

---------------------------------------------------------------------
-- html for tables of contents with only selected citations

rankListContents :: [T.Content] -> Text
-- ^Construct html for all rank list elements in the content list.
rankListContents = Tx.unlines . fst . foldl' go ([],1)
    where go (xs,n) (T.Content sel _ cs) =
             ( xs <> zipWith (rankListElement sel) [n..] cs, length cs + n )

rankListElement :: T.Selection -> Int -> T.Citation -> Text
-- ^Construct html for a citation element of a rank list.
rankListElement sel index cite = fill dict Temp.citationHtml
    where dict = rankListElementDict index sel cite

---------------------------------------------------------------------
-- html template dictionaries for citations

rankListElementDict :: Int -> T.Selection -> T.Citation -> Map.Map Text Text
-- ^html template dictionary for rank list element.
rankListElementDict index sel c = Map.union m . citationDict sel $ c
    where m = Map.fromList [ ( "index", C.tshow index        )
                           , ( "length", Vc.citationLength c )
                           , ( "type",   "text"              )
                           , ( "class",  "_citation"         )
                           ]

citationDict :: T.Selection -> T.Citation -> Map.Map Text Text
-- ^Basic html template for a citation.
citationDict (T.Selection iss _) c = Map.fromList xys
    where xys = [ ("id",       T.pmid c                        )
                , ("class",    className iss                   )
                , ("type",     "checkbox"                      )
                , ("href",     T.doi c                         )
                , ("title",    T.title       $ c               )
                , ("authors",  Vc.authorLine $ c               )
                , ("journal",  T.name  . T.journal $ iss       )
                , ("volume",   C.tshow . T.volNo   $ iss       )
                , ("number",   C.tshow . T.issNo   $ iss       )
                , ("pages",    C.tshow . T.pages   $ c         )
                , ("pmid",     T.pmid c                        )
                ]

-- --------------------------------------------------------------- --
-- Javascript elements for building the selection text file

savePrefix :: T.HasIssue a => T.JSet a -> Text
-- ^Filename prefix for the selection text file.
savePrefix jset = "sel" <> C.tshow y <> "-" <> ( C.tshow . T.setNo $ jset )
    where y = T.year jset
