{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module View.View
    ( runView
      -- Viewing journal sets
    , jsetsToCsv
    , jsetToCsv
    , jsetsToTxt
    , jsetToTxt
    , jsetsToMkd
    , jsetToMkd
      -- Viewing Issues
    , showIssue
    , viewIssue
    , viewIssueMkd
      -- Viewing issue contents (tables of contents)
    , viewRanks
    , tocsToTxt
    , tocToTxt
    , tocsToMkd
    , tocToMkd
    , tocsToHtml
      -- Viewing selections
    , selectionsToTxt
    , selectionToTxt
      -- Viewing references
    , referenceToTxt
    ) where

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Core  as C
import qualified Model.Core.Types as T
import qualified Model.Journals   as J
import qualified View.Core        as Vc
import qualified View.Html        as Html
import qualified View.Templates   as Temp
import           Data.Text                ( Text                 )
import           Data.List                ( sortBy, intersperse  )
import           Control.Monad.Reader     ( ask, asks, runReader )
import           Control.Monad.Writer     ( execWriterT, tell    )
import           Data.Monoid              ( Endo (..), appEndo   )
import           Data.Ord                 ( comparing            )
import           View.Templates           ( fill                 )

-- =============================================================== --
-- Viewer

runView :: T.ViewMonad a -> T.AppMonad Text
runView view = ask >>= pure . Tx.concat . flip appEndo [] . run
    where run config = flip runReader config . execWriterT $ view

write :: Text -> T.ViewMonad ()
write x = tell $ Endo ( [x] <> )

newLine :: T.ViewMonad ()
newLine = write "\n"

space :: T.ViewMonad ()
space = write " "

writeLn :: Text -> T.ViewMonad ()
writeLn x = write x *> newLine

writeLns :: [Text] -> T.ViewMonad ()
writeLns = mapM_ writeLn

writeLns' :: [Text] -> T.ViewMonad ()
writeLns' = separate newLine . map write

separate :: T.ViewMonad a -> [T.ViewMonad a] -> T.ViewMonad ()
separate sep = sequence_ . intersperse sep

format :: T.ViewMonad T.Format
format = asks ( fmap C.extension . T.cOutputPath )
         >>= \case Just "txt"  -> pure T.TXT
                   Just "csv"  -> pure T.CSV
                   Just "html" -> pure T.HTML
                   Just "mkd"  -> pure T.MKD
                   Just "md"   -> pure T.MKD
                   _           -> pure T.TXT

-- =============================================================== --
-- Viewing journal sets

---------------------------------------------------------------------
-- As CSV

jsetsToCsv :: T.HasIssue a => [Text] -> T.JSets a -> T.ViewMonad ()
-- ^Convert a collection of journal sets to CSV. Every element is
-- enclosed in double quotes. The first line is the list of journals
-- by journal abbreviation as specified by the first argument. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsToCsv []    _     = pure ()
jsetsToCsv abbrs jsets = do
    write "No. & Date,"
    writeLn . Tx.intercalate "," $ abbrs
    mapM_ ( jsetToCsv abbrs ) . J.unpack $ jsets

jsetToCsv :: T.HasIssue a => [Text] -> T.JSet a -> T.ViewMonad ()
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journal abbreviations in the order they will be
-- tabulated. The second argument is the journal set. The first cell
-- will be the key associated with the journal set. Subsequent cells
-- will list the issues for the corresponding journal separated by
-- newline characters. All elements are enclosed in double quotes.
jsetToCsv []    _    = pure ()
jsetToCsv abbrs jset = do
    write "\""
    write . C.tshow . T.setNo $ jset
    newLine
    write . Vc.dateN $ jset
    write "\","
    separate (write ",") . map ( jsetIssuesToCsv jset ) $ abbrs
    newLine

jsetIssuesToCsv :: T.HasIssue a => T.JSet a -> Text -> T.ViewMonad ()
jsetIssuesToCsv jset abbr = do
    write "\""
    writeLns' . map Vc.volIss . J.issuesByAbbr abbr . T.issues $ jset
    write "\""

---------------------------------------------------------------------
-- As Text

jsetsToTxt :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
jsetsToTxt (T.JSets jsets) = mapM_ jsetToTxt jsets

jsetToTxt :: T.HasIssue a => T.JSet a -> T.ViewMonad ()
jsetToTxt jset = do
    let issues = sortBy (comparing $ T.name . T.journal) . T.issues $ jset
    writeLn $ Vc.jsetHeader jset
    mapM_ viewIssue issues
    newLine

---------------------------------------------------------------------
-- to Markdown

jsetsToMkd :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
jsetsToMkd = mapM_ jsetToMkd . J.unpack

jsetToMkd :: T.HasIssue a => T.JSet a -> T.ViewMonad ()
jsetToMkd jset = do
    writeLn $ "## " <> Vc.jsetHeader jset
    newLine
    mapM_ ( \ iss -> write "* " *> viewIssueMkd iss ) . T.issues $ jset
    newLine

-- =============================================================== --
-- Viewing issues

showIssue :: T.HasIssue a => a -> Text
showIssue iss = Tx.unwords . map ($iss) $ parts
    where parts = [ T.abbr . T.journal, Vc.volIss, Vc.dateP ]

viewIssue :: T.HasIssue a => a -> T.ViewMonad ()
-- ^Format an issue as "abbr volume number (year-month-day)".
viewIssue iss = do
    write . T.abbr . T.journal $ iss
    space
    write . Vc.volIss $ iss
    space
    write . Vc.dateP $ iss
    newLine

viewIssueMkd :: T.HasIssue a => a -> T.ViewMonad ()
viewIssueMkd iss = do
    write . T.name . T.journal $ iss
    space
    write . Vc.volIss $ iss
    write ","
    space
    write . Vc.dateW $ iss
    newLine

-- =============================================================== --
-- Viewing citations

viewCitation :: T.HasIssue a => a -> T.Citation -> T.ViewMonad ()
viewCitation iss c = do
    writeLn . T.title $ c
    writeLn . Vc.authorLine $c
    write . T.name . T.journal $ iss
    space
    write . Vc.volIss $ iss
    write ","
    space
    write . Vc.pageRange $ c
    newLine

citationToMkd :: T.Selection -> T.Citation -> T.ViewMonad ()
citationToMkd sel x = write . fill dict $ Temp.citationMkd
    where dict = Map.fromList [ ( "title",   Vc.mkdBrackets . T.title $ x )
                              , ( "doi",     T.doi x                      )
                              , ( "authors", Vc.authorLine x              )
                              , ( "journal", T.name . T.journal $ sel     )
                              , ( "volIss",  Vc.volIss sel                )
                              , ( "pages",   Vc.pageRange x               )
                              , ( "pmid",    T.pmid x                     )
                              ]

-- =============================================================== --
-- Viewing Issue Contents (tables of contents and ranking lists)

viewRanks :: T.JSet T.IssueContent -> T.ViewMonad ()
viewRanks jset@(T.JSet _ ics) = do
    name  <- maybe "Somebody" id . C.choice <$> mapM asks [T.cNick, T.cUser]
    email <- asks $ maybe "their email address" id . T.cEmail
    format >>= \case
        T.HTML -> write . Html.htmlToCRank name email $ jset
        T.MKD  -> mapM_ tocToMkd ics
        _      -> mapM_ tocToTxt ics

---------------------------------------------------------------------
-- As Text

tocsToTxt :: T.JSet T.IssueContent -> T.ViewMonad ()
tocsToTxt (T.JSet _ cs) = separate newLine . map tocToTxt $ cs

tocToTxt :: T.IssueContent -> T.ViewMonad ()
tocToTxt (T.IssueContent sel cs) = do
    viewIssue sel
    newLine
    separate newLine . map (viewCitation sel) $ cs

---------------------------------------------------------------------
-- As Markdown

tocsToMkd :: T.JSet T.IssueContent -> T.ViewMonad ()
tocsToMkd (T.JSet setNo cs) = do
    write "# Journal Set "
    writeLn . C.tshow $ setNo
    mapM_ tocToMkd cs

tocToMkd :: T.IssueContent -> T.ViewMonad ()
tocToMkd (T.IssueContent x cs) = do
    write "## "
    viewIssueMkd x
    newLine
    if null cs
       then writeLn "No citations listed at PubMed."
       else separate newLine . map (citationToMkd x) $ cs

---------------------------------------------------------------------
-- As HTML

tocsToHtml :: T.JSet T.IssueContent -> T.ViewMonad ()
tocsToHtml jset = do
    style <- asks T.cToCStyle
    name  <- maybe "Somebody" id . C.choice <$> mapM asks [T.cNick, T.cUser]
    email <- asks $ maybe "their email address" id . T.cEmail
    case style of
         T.Select  -> write . Html.htmlToCSelect  name email $ jset
         T.Rank    -> write . Html.htmlToCRank    name email $ jset
         T.Propose -> write . Html.htmlToCPropose name email $ jset

-- =============================================================== --
-- Formatting selection sets

---------------------------------------------------------------------
-- As text

selectionsToTxt :: T.JSets T.Selection -> Text
selectionsToTxt (T.JSets jsets) = Tx.intercalate "\n"
                                  . map selectionToTxt
                                  $ jsets

selectionToTxt :: T.JSet T.Selection -> Text
selectionToTxt jset@(T.JSet _ xs) =
    let go x = showIssue x : map ( Tx.append "    " ) (T.selected x)
    in  Tx.unlines $ Vc.jsetHeader jset : concatMap go xs

-- =============================================================== --
-- Formatting journals and reference issues

referenceToTxt :: T.Issue -> Text
referenceToTxt x = Tx.unlines hs
    where j  = T.journal x
          hs = [ T.name j
               , "  abbr:      " <> T.abbr j
               , "  pubmed:    " <> T.pubmed j
               , "  frequency: " <> (freqToTxt . T.freq) j
               , "  resets:    " <> (resetsToTxt . T.resets) j
               , "  reference: " <> showIssue x
               ]

resetsToTxt :: Bool -> Text
resetsToTxt True  = "yes (issue numbers reset to 1 each year)"
resetsToTxt False = "no (issue numbers continuously increase)"

freqToTxt :: T.Frequency -> Text
freqToTxt T.Weekly      = "weekly (52 issues per year)"
freqToTxt T.WeeklyLast  = "weekly-last (drop the last issue of the year)"
freqToTxt T.WeeklyFirst = "weekly-first (drop the first issue of the year)"
freqToTxt T.Monthly     = "monthly (12 issues per year)"
