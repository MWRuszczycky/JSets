{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.CSV
    ( CSV
    , parse
    ) where

import qualified Data.Attoparsec.Text as At
import qualified Model.Parsers.Core   as P
import           Data.Text                   ( Text        )
import           Data.Bifunctor              ( bimap       )
import           Control.Applicative         ( (<|>), many )

-- =============================================================== --
-- Types

type CSV = [[Text]]

-- =============================================================== --
-- Exported parsers

parse :: Text -> Either String CSV
parse = bimap err id . At.parseOnly csv
    where err = (<>) "Cannot parse CSV: "

-- =============================================================== --
-- CSV Parser components

csv :: At.Parser CSV
csv = many csvLine <* At.endOfInput

csvLine :: At.Parser [Text]
csvLine = At.sepBy csvCell (At.char ',') <* At.endOfLine

csvCell :: At.Parser Text
csvCell = P.quoted <|> At.takeTill endOfCell
    where endOfCell c = c == ',' || c == '\n' || c == '\r'
