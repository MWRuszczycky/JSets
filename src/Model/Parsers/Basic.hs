{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Basic
    ( JSON      (..)
    , CSV
    , parseCSV
    , parseJSON
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text              )
import           Data.Bifunctor              ( bimap             )
import           Control.Applicative         ( (<|>), many, some )

-- =============================================================== --
-- Types

data JSON =
      JStr   Text
    | JNum   Text
    | JInt   Integer
    | JDbl   Double
    | JBool  Bool
    | JNull
    | JObj   [ (Text, JSON) ]
    | JArray [ JSON ]
      deriving ( Show )

type CSV = [[Text]]

-- =============================================================== --
-- Exported parsers

parseCSV :: Text -> Either String CSV
parseCSV = bimap err id . At.parseOnly csv
    where err = (++) "Cannot parse CSV: "

parseJSON :: Text -> Either String JSON
parseJSON = bimap err id . At.parseOnly json
    where err = (++) "Cannot parse JSON: "

-- =============================================================== --
-- General parsers

quoted :: At.Parser Text
quoted = do
    At.char '\"'
    s <- many $ escaped <|> At.satisfy ( \ x -> x /= '\\' && x /= '\"' )
    At.char '\"'
    pure . Tx.pack $ s

escaped :: At.Parser Char
escaped = At.char '\\' *> At.choice [ At.char '\"' *> pure '\"'
                                    , At.char '/'  *> pure '/'
                                    , At.char 'b'  *> pure '\b'
                                    , At.char 'f'  *> pure '\f'
                                    , At.char 'n'  *> pure '\n'
                                    , At.char 'r'  *> pure '\r'
                                    , At.char 't'  *> pure '\t'
                                    , At.char 'u'  *> pure 'u'
                                    , At.char '\\' *> pure '\\'
                                    ]

comma :: At.Parser ()
comma = At.skipSpace *> At.char ',' *> At.skipSpace

leftBrace :: At.Parser ()
leftBrace = At.skipSpace *> At.char '{' *> At.skipSpace

rightBrace :: At.Parser ()
rightBrace = At.skipSpace *> At.char '}' *> At.skipSpace

leftBracket :: At.Parser ()
leftBracket = At.skipSpace *> At.char '[' *> At.skipSpace

rightBracket :: At.Parser ()
rightBracket = At.skipSpace *> At.char ']' *> At.skipSpace

colon :: At.Parser ()
colon = At.skipSpace *> At.char ':' *> At.skipSpace

-- =============================================================== --
-- CSV Parser components

csv :: At.Parser CSV
csv = many csvLine <* At.endOfInput

csvLine :: At.Parser [Text]
csvLine = At.sepBy csvCell (At.char ',') <* At.endOfLine

csvCell :: At.Parser Text
csvCell = quoted <|> At.takeTill endOfCell
    where endOfCell c = c == ',' || c == '\n' || c == '\r'

-- =============================================================== --
-- JSON parser components

json :: At.Parser JSON
json = jsonObject <* At.endOfInput

jsonObject :: At.Parser JSON
jsonObject = do
    leftBrace
    kvps <- At.sepBy jsonKeyValuePair comma
    rightBrace
    pure . JObj $ kvps

jsonKeyValuePair :: At.Parser (Text, JSON)
jsonKeyValuePair = do
    key <- quoted
    colon
    val <- jsonValue
    pure (key, val)

jsonValue :: At.Parser JSON
jsonValue = jsonString
            <|> jsonObject
            <|> jsonArray
            <|> jsonInteger
            <|> jsonDouble
            <|> jsonBool
            <|> jsonNull

jsonString :: At.Parser JSON
jsonString = quoted >>= pure . JStr

jsonArray :: At.Parser JSON
jsonArray = do
    leftBracket
    vals <- At.sepBy jsonValue comma
    rightBracket
    pure . JArray $ vals

jsonDouble :: At.Parser JSON
-- ^Does not exactly follow the json specification, because it is
-- more permissive. For example, 09 will and +9 will both parse as
-- 9.0 when the parser should fail. See
-- https://www.json.org/json-en.html
jsonDouble = JDbl <$> At.double

jsonInteger :: At.Parser JSON
jsonInteger = do
    x  <- At.char '-' <|> At.satisfy (At.inClass "123456789")
    xs <- if x == '-' then some At.digit else many At.digit
    pure . JInt . read $ x : xs

jsonBool :: At.Parser JSON
jsonBool = trueBool <|> falseBool
    where trueBool  = At.string "true"  *> pure (JBool True)
          falseBool = At.string "false" *> pure (JBool False)

jsonNull :: At.Parser JSON
jsonNull = At.string "null" *> pure JNull
