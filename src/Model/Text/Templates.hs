{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model.Text.Templates
    ( TemplateText (..)
    , Template
    , fill
    , fillDef
    , parseTemplate
      -- Templates
      -- html
    , citationTemplate
    , issueTemplate
    , tocsTemplate
    , tocTemplate
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import qualified Data.FileEmbed       as FE
import qualified Data.Map.Strict      as Map
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
-- 'AtVar'. The open brace must immediately follow the @ symbol,
-- otherwise it is parsed as an @-symbol (FreeAt). Everything else
-- parses as AtRaw. Fails on unclosed braces after @{ open braces.
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

tocsTemplate :: Template
-- full html document for table of contents selections
-- jsetTitle  : title of html document
-- jsetHeader : header for te journal set (number and date)
-- savePrefix : file name prefix for saving
-- newIssues  : new issue elements for the 'journals' array
-- tocs       : table of contents html for all issues
tocsTemplate = parseTemplate' "res/html/tocsTemplate.html"
               $(FE.embedStringFile "res/html/tocsTemplate.html")

issueTemplate :: Template
-- element of the 'issues array'
-- class  : issue class
-- title  : Journal title
-- vol    : issue volume
-- number : issue number
-- date   : issue date
issueTemplate = parseTemplate' "tIssueHtml-template" . Tx.unwords $ t
    where t = [ Tx.replicate 22 " "
               , "new JournalIssue(\"@{class}\","
               , "\"@{title}\","
               , "\"@{vol}\","
               , "\"@{number}\","
               , "\"@{date}\")"
               ]

tocTemplate :: Template
-- table of contents for a give issue
-- issue    : issue header
-- articles : html for all articles in the issue
tocTemplate = parseTemplate' "res/html/tocTemplate.html"
              $(FE.embedStringFile "res/html/tocTemplate.html")

citationTemplate :: Template
-- <p> environment for a single article citation
-- id      : article id
-- class   : issue class
-- href    : article doi link
-- title   : article title
-- authors : article authors
-- journal : journal name
-- volume  : issue volume
-- number  : issue number
-- pages   : pages string
citationTemplate = parseTemplate' "res/html/citationTemplate.html"
                   $(FE.embedStringFile "res/html/citationTemplate.html")
