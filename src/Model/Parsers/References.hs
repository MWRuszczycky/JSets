{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.References
    ( parseReferences
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Data.Text            as Tx
import qualified Data.Time            as Tm
import qualified Data.Text.IO         as Tx
import qualified Data.Char            as Ch
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text              )
import           Data.List                   ( nub               )
import           Control.Applicative         ( (<|>), many, some )

-- =============================================================== --
-- Testing

loadTest :: IO Text
loadTest = Tx.readFile "../dev/references.txt"

-- =============================================================== --
-- Parser

parseReferences :: Text -> Either T.ErrString [T.Issue]
parseReferences txt = handleParseResult txt . At.parse refDicts $ txt

handleParseResult :: Text -> At.Result [Dict] -> Either T.ErrString [T.Issue]
handleParseResult xs (At.Fail ys _  _) = handleParseFail xs ys
handleParseResult xs (At.Partial go  ) = handleParseResult xs . go $ Tx.empty
handleParseResult _  (At.Done _ r    ) = validate r

handleParseFail :: Text -> Text -> Either T.ErrString [T.Issue]
handleParseFail input rest = Left msg
    where n   = length . Tx.lines $ input
          m   = length . Tx.lines $ rest
          msg = unwords [ "Parse failure at line "
                        , show (n - m + 1) <> " : "
                        , Tx.unpack . Tx.takeWhile (not . Ch.isSpace) $ rest
                        ]

-- =============================================================== --
-- Local types

type Dict = [(Text, Text)]

-- =============================================================== --
-- Reference issue validation

validate :: [Dict] -> Either T.ErrString [T.Issue]
validate ds = mapM validateRef ds >>= checkForDuplicates

checkForDuplicates :: [T.Issue] -> Either T.ErrString [T.Issue]
checkForDuplicates xs
    | ys == nub ys = pure xs
    | otherwise    = Left "References have repeated journal abbreviations!"
    where ys = map (T.name . T.journal) xs

validateRef :: Dict -> Either T.ErrString T.Issue
validateRef d = T.Issue <$> getDate     d
                        <*> getIntValue d "volume"
                        <*> getIntValue d "issue"
                        <*> getJournal  d

getJournal :: Dict -> Either T.ErrString T.Journal
getJournal d = do
    (j, k) <- getJournalAbbr d
    validateString j
    validateString k
    T.Journal k j <$> getStringValue d "pubmed"
                  <*> getFrequency   d
                  <*> getResets      d

getFrequency :: Dict -> Either T.ErrString T.Frequency
getFrequency = maybe err go . lookup "frequency"
    where err  = Left $ "Missing or invalid frequency!"
          go x = case Tx.map Ch.toLower . Tx.strip $ x of
                      "weekly"       -> pure T.Weekly
                      "weekly-first" -> pure T.WeeklyFirst
                      "weekly-last"  -> pure T.WeeklyLast
                      "monthly"      -> pure T.Monthly
                      _              -> err

getDate :: Dict -> Either T.ErrString Tm.Day
getDate dict = do
    d <- getIntValue dict "day"
    y <- fromIntegral <$> getIntValue dict "year"
    m <- getMonth    dict
    pure $ Tm.fromGregorian y m d

getMonth :: Dict -> Either T.ErrString Int
getMonth = maybe err go . lookup "month"
    where err  = Left "Missing <month> field!"
          go x = case Tx.map Ch.toLower . Tx.strip $ x of
                      "january"     -> pure 1
                      "february"    -> pure 2
                      "march"       -> pure 3
                      "april"       -> pure 4
                      "may"         -> pure 5
                      "june"        -> pure 6
                      "july"        -> pure 7
                      "august"      -> pure 8
                      "september"   -> pure 9
                      "october"     -> pure 10
                      "november"    -> pure 11
                      "december"    -> pure 12
                      u             -> Left $ "Invalid month: " <> Tx.unpack u

getResets :: Dict -> Either T.ErrString Bool
getResets = maybe err go . lookup "resets"
    where err  = Left "Missing or invalid <resets> value!"
          go x = case Tx.map Ch.toLower . Tx.strip $ x of
                       "true"  -> pure True
                       "false" -> pure False
                       _       -> err

getJournalAbbr :: Dict -> Either T.ErrString (Text, Text)
getJournalAbbr =  maybe (err "Missing header.") go . lookup "journal"
    where err y = Left $ "Cannot parse journal header!" <> y
          go x  = case break (== '/') . Tx.unpack $ x of
                       ([]  , _      ) -> err " Missing journal name!"
                       (_   ,'/':[]  ) -> err " Missing journal abbreviation!"
                       (name,'/':abbr) -> pure (Tx.pack name, Tx.pack abbr)
                       (_   , _      ) -> err " Missing journal abbreviation!"

getStringValue :: Dict -> Text -> Either T.ErrString Text
getStringValue d k = maybe err go . lookup k $ d
    where go x = validateString x
          err  = Left $ "A record lacks a <" <> Tx.unpack k <> "> field!"

getIntValue :: Dict -> Text -> Either T.ErrString Int
getIntValue d k = getStringValue d k >>= go
    where go   = maybe err pure . C.readMaybeTxt
          err  = Left $ "A record requires an integer value for the <"
                        <> Tx.unpack k <> "> field!"

validateString :: Text -> Either T.ErrString Text
validateString x
    | allValid  = pure x
    | otherwise = Left errMsg
    where allValid = Tx.all ( \ c -> Ch.isAlphaNum c || c == '_' || c == ' ' ) x
          errMsg   = "The field value '" <> Tx.unpack x
                     <> "' contains invalid characters!"

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
