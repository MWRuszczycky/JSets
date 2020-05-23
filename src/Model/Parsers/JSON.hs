{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JSON
    ( JSON      (..)
    , parse
    , lookupWith
    , lookupListWith
    , obj
    , array
    , str
    ) where

import qualified Data.Attoparsec.Text as At
import qualified Model.Parsers.Core   as P
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

-- =============================================================== --
-- JSON utilities

lookupWith :: [Text] -> (JSON -> Maybe a) -> JSON -> Maybe a
lookupWith []     f json        = f json
lookupWith (x:xs) f (JObj dict) = lookup x dict >>= lookupWith xs f
lookupWith _      _ _           = Nothing

lookupListWith :: [Text] -> (JSON -> Maybe a) -> JSON -> Maybe [a]
lookupListWith xs f json = lookupWith xs array json >>= mapM f

obj :: JSON -> Maybe [(Text, JSON)]
obj (JObj json) = pure json
obj _           = Nothing

array :: JSON -> Maybe [JSON]
array (JArray js) = pure js
array _           = Nothing

str :: JSON -> Maybe Text
str (JStr txt) = pure txt
str _          = Nothing

-- =============================================================== --
-- Parsers

parse :: Text -> Either String JSON
parse = bimap err id . At.parseOnly ( jsonObject <* At.endOfInput )
    where err = (<>) "Cannot parse JSON: "

---------------------------------------------------------------------
-- Parser components

jsonObject :: At.Parser JSON
jsonObject = do
    P.leftBrace
    kvps <- At.sepBy jsonKeyValuePair P.comma
    P.rightBrace
    pure . JObj $ kvps

jsonKeyValuePair :: At.Parser (Text, JSON)
jsonKeyValuePair = do
    key <- P.quoted
    P.colon
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
jsonString = P.quoted >>= pure . JStr

jsonArray :: At.Parser JSON
jsonArray = do
    P.leftBracket
    vals <- At.sepBy jsonValue P.comma
    P.rightBracket
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
