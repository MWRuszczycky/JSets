{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module View.Templates
    ( TemplateText (..)
    , Template
    , fill
    , fillNone
    , fillDef
    , parseTemplate
      -- Templates
      -- html/JavaScript
    , citationHtml
    , issueJS
    , tocsHtml
    , tocHtml
    , tocMissingHtml
    , tocMissingUrlHtml
    , saveInstrHtml
    , tocInstrHtml
    , rankListHtml
      -- markdown
    , citationMkd
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict      as Map
import qualified Model.Core.CoreTH    as MT
import           Data.Text                   ( Text               )
import           Control.Applicative         ( (<|>), empty, many )

-- =============================================================== --
-- Local types

data TemplateText = FreeAt | Var Text | Raw Text deriving ( Eq, Show )

type Template = [TemplateText]

-- =============================================================== --
-- Text interpolation for Templated Text strings

fill :: Map.Map Text Text -> Template -> Text
fill = fillDef Tx.empty

fillNone :: Template -> Text
fillNone = fillDef Tx.empty Map.empty

fillDef :: Text -> Map.Map Text Text -> Template -> Text
fillDef def dict = Tx.concat . map go
    where go FreeAt  = "@"
          go (Var x) = Map.findWithDefault def x dict
          go (Raw x) = x

-- =============================================================== --
-- Parser

parseTemplate' :: String -> Text -> Template
parseTemplate' fp = either error id . parseTemplate fp

parseTemplate :: String -> Text -> Either String Template
-- ^Parses at-braced text. Anything in "@{key}" is parsed out as
-- 'Var'. The open brace must immediately follow the @ symbol,
-- otherwise it is parsed as an @-symbol (FreeAt). Everything else
-- parses as Raw. Fails on unclosed braces after @{ open braces.
parseTemplate fp txt = go . At.parse atText $ txt
    where go (At.Partial cont  ) = go . cont $ Tx.empty
          go (At.Done xs r     ) | Tx.null xs = pure r
                                 | otherwise  = go $ At.Fail xs [] ""
          go (At.Fail xs _    _) = let n = length . Tx.lines $ txt
                                       m = length . Tx.lines $ xs
                                   in  Left $ templateFailStr fp $ n - m + 1

templateFailStr :: String -> Int -> String
templateFailStr fp n = unwords hs
    where hs = [ "parseAt failed to parse!"
               , fp <> ", line " <> show n <> ":"
               , "likely unclosed at-brace."
               ]

atText :: At.Parser Template
atText = many ( At.peekChar' >>= go ) <* At.endOfInput
    where go '@' = freeAt <|> var
          go  _  = raw

raw :: At.Parser TemplateText
raw = Raw <$> At.takeTill (=='@')

var :: At.Parser TemplateText
var = Var <$> go
    where go = At.string "@{" *> At.takeTill (=='}') <* At.char '}'

freeAt :: At.Parser TemplateText
freeAt = do
    At.char '@'
    nxt <- At.peekChar
    case nxt of
         Just '{' -> empty
         _        -> pure FreeAt

-- =============================================================== --
-- Templates

---------------------------------------------------------------------
-- html templates for building tables of contents web documents

tocsHtml :: Template
-- ^Full html document for table of contents selections
-- jsetTitle  : title of html document
-- jsetHeader : header for te journal set (number and date)
-- savePrefix : file name prefix for saving
-- issues     : new issue elements for the 'journals' array
-- tocs       : table of contents html for all issues
-- saveinstr  : save instructions
tocsHtml = parseTemplate' "res/html/tocs.html"
           $(MT.embedFile "res/html/tocs.html")

issueJS :: Template
-- ^Element of the 'issues array'
-- class  : issue class
-- title  : Journal title
-- vol    : issue volume
-- number : issue number
-- date   : issue date
issueJS = parseTemplate' "tIssueHtml-template" . Tx.unwords $ t
    where t = [ Tx.replicate 22 " "
              , "new JournalIssue(\"@{class}\","
              , "\"@{title}\","
              , "\"@{vol}\","
              , "\"@{number}\","
              , "\"@{date}\")"
              ]

tocHtml :: Template
-- ^Table of contents for a given issue
-- issue    : issue header
-- articles : html for all articles in the issue
tocHtml = parseTemplate' "res/html/toc.html"
          $(MT.embedFile "res/html/toc.html")

tocMissingHtml :: Template
-- ^Table of contents substitute when there are no PMIDs at PubMed
-- issue : issue header
tocMissingHtml = parseTemplate' "res/html/tocMissing.html"
                 $(MT.embedFile "res/html/tocMissing.html")

tocMissingUrlHtml :: Template
-- ^Same as tocMissingHtml but with an alternate link to the table of
-- contents at the publisher's website.
-- issue : issue header
-- url   : url to the toc at the publisher's website.
tocMissingUrlHtml = parseTemplate' "res/html/tocMissingUrl.html"
                    $(MT.embedFile "res/html/tocMissingUrl.html")

citationHtml :: Template
-- ^Paragraph environment for a single article citation
-- selected : optional class for selected citations
-- id       : article id
-- class    : issue class
-- href     : article doi link
-- title    : article title
-- authors  : article authors
-- journal  : journal name
-- volume   : issue volume
-- number   : issue number
-- pages    : pages string
-- pmid     : pubmed uid
citationHtml = parseTemplate' "res/html/citation.html"
               $(MT.embedFile "res/html/citation.html")

saveInstrHtml :: Template
-- ^Save instructions for Table of Contents with instructions.
-- name  : nickname of person to send selections to
-- email : email string for person to send selections to
saveInstrHtml = parseTemplate' "res/html/saveInstr.html"
                $(MT.embedFile "res/html/saveInstr.html")

tocInstrHtml :: Template
-- ^General instructions for table of contents to be included at the
-- top of the webpage when the --instruct option is used.
tocInstrHtml = parseTemplate' "res/html/tocInstr.html"
               $(MT.embedFile "res/html/tocInstr.html")

rankListHtml :: Template
-- ^Full html template document for generating rankings for articles.
-- selected for review.
-- jsetTitle : Title of the journal set (e.g., Journal Set 6)
-- name      : Nickname of person to send rankings to
-- email     : email string for person to send rankings to
-- citations : Individual citations for ranking.
rankListHtml = parseTemplate' "res/html/rankList.html"
               $(MT.embedFile "res/html/rankList.html")

---------------------------------------------------------------------
-- Markdown templates

citationMkd :: Template
-- ^Template for citations in Markdown.
-- title   : title of journal
-- doi     : doi of citation
-- authors : authors of the cited article
-- journal : journal name
-- volIss  : volume issue pair
-- pages   : page range
-- pmid    : pubmed id
citationMkd = parseTemplate' "res/mkd/citation.mkd"
              $(MT.embedFile "res/mkd/citation.mkd")
