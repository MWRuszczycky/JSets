{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.References
    (
    ) where

import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import qualified Data.Text.IO         as Tx
import qualified Data.Char            as Ch
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text              )
import           Data.Bifunctor              ( bimap             )
import           Control.Applicative         ( (<|>), many, some )

-- =============================================================== --
-- Testing

loadTest :: IO Text
loadTest = Tx.readFile "../dev/references.txt"

-- =============================================================== --
-- Parser

parse :: Text -> Either T.ErrString [Dict]
parse txt = handleParseResult txt . At.parse refDicts $ txt

handleParseResult :: Text -> At.Result [Dict] -> Either T.ErrString [Dict]
handleParseResult xs (At.Fail ys _  _) = handleParseFail xs ys
handleParseResult xs (At.Partial go  ) = handleParseResult xs . go $ Tx.empty
handleParseResult _  (At.Done _ r    ) = validate r

handleParseFail :: Text -> Text -> Either T.ErrString [Dict]
handleParseFail input rest = Left msg
    where n   = length . Tx.lines $ input
          m   = length . Tx.lines $ rest
          msg = unlines [ "Parse failure at line "
                        , show (n - m + 1) <> " : "
                        , Tx.unpack . Tx.takeWhile (not . Ch.isSpace) $ rest
                        ]

-- =============================================================== --
-- Local types

type Dict = [(Text, Text)]

-- =============================================================== --
-- Reference issue validation

validate :: [Dict] -> Either T.ErrString [Dict] -- will be [T.Issue]
validate = pure

validFields :: [Text]
validFields = [ "day"
              , "frequency"
              , "issue"
              , "month"
              , "pubmed"
              , "resets"
              , "volume"
              , "year"
              ]

-- =============================================================== --
-- Components

---------------------------------------------------------------------
-- Helpers

comment :: At.Parser ()
comment = At.char '#' *> At.takeTill At.isEndOfLine *> At.skipSpace

skipComments :: At.Parser ()
skipComments = At.skipSpace *> many comment *> pure ()

keyValuePair :: Text -> At.Parser (Text, Text)
keyValuePair key = do
    skipComments
    k <- At.string key
    skipComments
    At.char ':'
    skipComments
    v <- validValue
    comment <|> At.endOfLine
    pure (k,v)

validValue :: At.Parser Text
validValue = fmap Tx.pack . some . At.satisfy $ At.notInClass ":#\n\r\t"

field :: At.Parser (Text, Text)
field = At.choice $ map keyValuePair validFields

---------------------------------------------------------------------
-- Reference Dict parsers

refDicts :: At.Parser [Dict]
refDicts = do
    skipComments
    ds <- many refDict
    skipComments
    At.endOfInput
    pure ds

refDict :: At.Parser Dict
refDict = do
    jh <- keyValuePair "journal"
    fs <- some field
    skipComments
    pure $ jh : fs
