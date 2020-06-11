{-# LANGUAGE OverloadedStrings #-}

module View.Core
    ( -- General formatting functions
      bracket
    , spaceToUnder
    , dateN
    , dateP
    , dateW
    , showPicoSec
      -- Text formatting of journal sets, issues, citations...
    , jsetHeader
    , jsetVHeader
    , volIss
    , authorLine
    , pageRange
    , citationLength
      -- Markdown
    , mkdBrackets
    , mkdBd
    , mkdIt
    , mkdLink
      -- ViewMonad actions
    , write
    , writeLn
    , writeLns
    , writeLns'
    , newLine
    , space
    , separate
    , prepend
    ) where

import qualified Data.Text            as Tx
import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import           Data.Time                  ( toGregorian )
import           Data.Text                  ( Text        )
import           Data.Char                  ( isSpace     )
import           Control.Monad.Writer       ( tell        )
import           Data.List                  ( intersperse )
import           Data.Monoid                ( Endo (..)   )

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

showPicoSec :: Integer -> Text
showPicoSec x
    | x < 0     = "<0 s"
    | msec < 1  = "<1 ms"
    | n < 1000  = Tx.pack $ secs <> "." <> msecs <> " s"
    | otherwise = ">1 ks"
    where msec       = truncate $ fromIntegral x / 10^9
          (n:ns)     = fst . unzip . tail . scanl go (0,x) $ [12,11 ..]
          secs       = show n
          msecs      = concatMap show . take (4 - length secs) $ ns
          go (_,v) n = let z = truncate $ fromIntegral v / 10^n
                       in  (z, v - z * 10^n)

-- =============================================================== --
-- Text formatting of Journal set, issue, citation, etc. components

---------------------------------------------------------------------
-- Journal sets

jsetHeader :: T.HasIssue a => T.JSet a -> Text
-- Formatted journal set header formotted with its availability date.
-- Dates are listed year-month-day. For example: "1 | 2020-04-30"
jsetHeader jset = Tx.unwords [ C.tshow . T.setNo $ jset
                             , "|"
                             , dateN $ jset
                             ]

jsetVHeader :: T.HasIssue a => T.JSet a -> Text
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

citationLength :: T.Citation -> Text
-- ^Calculation a citation length as short or long and the number of
-- pages. Online articles parse with page ranges from 1 to 0.
citationLength c
    | diff < 1  = " (online)"
    | diff < 6  = " (short: " <> C.tshow diff <> "p)"
    | otherwise = " (long: "  <> C.tshow diff <> "p)"
    where (T.PageNumber _ d0, T.PageNumber _ dn) = T.pages c
          diff = dn - d0 + 1

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

-- =============================================================== --
-- ViewMonad actions

write :: Text -> T.ViewMonad ()
write x = tell $ Endo ( [x] <> )

writeLn :: Text -> T.ViewMonad ()
writeLn x = write x *> newLine

writeLns :: [Text] -> T.ViewMonad ()
writeLns = mapM_ writeLn

writeLns' :: [Text] -> T.ViewMonad ()
writeLns' = separate newLine . map write

newLine :: T.ViewMonad ()
newLine = write "\n"

space :: T.ViewMonad ()
space = write " "

separate :: T.ViewMonad a -> [T.ViewMonad a] -> T.ViewMonad ()
separate sep = sequence_ . intersperse sep

prepend :: T.ViewMonad a -> [T.ViewMonad a] -> T.ViewMonad ()
prepend _   []     = pure ()
prepend cap (x:xs) = cap *> x *> prepend cap xs
