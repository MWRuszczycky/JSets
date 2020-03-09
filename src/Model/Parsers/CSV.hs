{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.CSV
    ( parseCSV
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text  )
import           Data.Bifunctor              ( bimap )
import           Control.Applicative         ( many  )

parseCSV :: Text -> Either String [[Text]]
parseCSV = bimap err id . At.parseOnly csv
    where err = (++) "Cannot parse CSV: "

csv :: At.Parser [[Text]]
csv = many csvLine <* At.endOfInput

csvLine :: At.Parser [Text]
csvLine = At.sepBy csvCell (At.char ',') <* At.endOfLine

csvCell :: At.Parser Text
csvCell = do
    x <- At.peekChar'
    case x of
         '\"' -> quoted
         _    -> At.takeTill $ flip elem [',', '\n', '\r']

quoted :: At.Parser Text
quoted = At.char '\"' *> go
    where go = do x <- At.anyChar
                  case x of
                       '\\' -> Tx.cons <$> At.anyChar <*> go
                       '\"' -> pure Tx.empty
                       _    -> Tx.cons x <$> go
