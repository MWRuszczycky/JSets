{-# LANGUAGE OverloadedStrings #-}

module View.Core
    ( -- General formatting functions
      bracket
    , spaceToUnder
    , dateN
    , dateP
    , dateW
      -- Text formatting of journal sets, issues, citations...
    , jsetHeader
    , jsetVHeader
    , volIss
    , authorLine
    , pageRange
      -- Markdown
    , mkdBrackets
    , mkdBd
    , mkdIt
    , mkdLink
    ) where

import qualified Data.Text            as Tx
import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import           Data.Time                  ( toGregorian )
import           Data.Text                  ( Text        )
import           Data.Char                  ( isSpace     )

-- =============================================================== --
-- General formatting functions

bracket :: Char -> Char -> Text -> Text
-- ^Place characters at the front and back of a text string.
bracket x y = flip Tx.snoc y . Tx.cons x

spaceToUnder :: Text -> Text
-- ^Convert spaces to underscores.
spaceToUnder = Tx.map go
    where go x | isSpace x = '_'
               | otherwise = x

dateN :: T.HasDate a => a -> Text
-- ^Format: "year-month-day". Example "1977-04-27".
dateN = C.tshow . T.date

dateP :: T.HasDate a => a -> Text
-- ^Format: "(year-month-day)". Example "(1977-04-27)".
dateP = bracket '(' ')' . C.tshow . T.date

dateW :: T.HasDate a => a -> Text
-- ^Format: "text-month day, year". Example "April 27, 1977".
dateW x = Tx.unwords [ go m, C.tshow d <> ",", C.tshow y ]
    where (y,m,d) = toGregorian . T.date $ x
          go 1    = "January"
          go 2    = "February"
          go 3    = "March"
          go 4    = "April"
          go 5    = "May"
          go 6    = "June"
          go 7    = "July"
          go 8    = "August"
          go 9    = "September"
          go 10   = "October"
          go 11   = "November"
          go 12   = "December"
          go _    = "??"

-- =============================================================== --
-- Text formatting of Journal set, issue, citation, etc. components

---------------------------------------------------------------------
-- Journal sets

jsetHeader :: T.HasIssue a => T.JournalSet a -> Text
-- Formatted journal set header formotted with its availability date.
-- Dates are listed year-month-day. For example: "1 | 2020-04-30"
jsetHeader jset = Tx.unwords [ C.tshow . T.setNo $ jset
                             , "|"
                             , dateN $ jset
                             ]

jsetVHeader :: T.HasIssue a => T.JournalSet a -> Text
-- Verbose formatting of journal set headers with availability date.
-- For example: "Journal Set 1 | April 30, 2020"
jsetVHeader jset = Tx.unwords [ "Journal Set"
                              , C.tshow . T.setNo $ jset
                              , "|"
                              , dateW jset
                              ]

---------------------------------------------------------------------
-- Issues

volIss :: T.HasIssue a => a -> Text
-- Format the volume and issue number of an issue as "vol:number"
volIss iss = Tx.intercalate ":" $ map C.tshow [ T.volNo iss, T.issNo iss ]

---------------------------------------------------------------------
-- Citations

authorLine :: T.Citation -> Text
authorLine c
    | null xs   = "No authors listed"
    | otherwise = Tx.intercalate ", " $ xs
    where xs = T.authors c

pageRange :: T.Citation -> Text
pageRange x = C.tshow p1 <> "-" <> C.tshow p2
    where (p1,p2) = T.pages x

-- =============================================================== --
-- Helper functions

---------------------------------------------------------------------
-- Markdown formatting

mkdBrackets :: Text -> Text
-- Replace brackets with similar characters compatible with Markdown.
mkdBrackets = Tx.map go
    where go x | x == '['  = toEnum 0x27e6
               | x == ']'  = toEnum 0x27e7
               | otherwise = x

mkdBd :: Text -> Text
-- ^Make text bold in Markdown.
mkdBd x = "**" <> x <> "**"

mkdIt :: Text -> Text
-- ^Make text itallic in Markdown.
mkdIt = bracket '*' '*'

mkdLink :: Text -> Text -> Text
-- ^Add a url link in Markdown.
mkdLink content url = contentMkd <> link
    where contentMkd = bracket '[' ']' content
          link       = bracket '(' ')' url
