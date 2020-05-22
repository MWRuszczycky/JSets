{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.References
    ( parseReferences
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Data.Text            as Tx
import qualified Data.Time            as Tm
import qualified Data.Char            as Ch
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text              )
import           Data.List                   ( nub, intercalate  )
import           Data.Bifunctor              ( bimap             )
import           Control.Applicative         ( (<|>), many, some )

-- =============================================================== --
-- Local types

type Dict = [(Text, Text)]

-- =============================================================== --
-- Parser

parseReferences :: String -> Text -> Either T.ErrString [T.Issue]
parseReferences fp txt = bimap (parseError fp) id
                         . handleParseResult txt
                         . At.parse refDicts
                         $ txt

handleParseResult :: Text -> At.Result [Dict] -> Either T.ErrString [T.Issue]
handleParseResult xs (At.Fail ys _  _) = handleParseFail xs ys
handleParseResult xs (At.Partial go  ) = handleParseResult xs . go $ Tx.empty
handleParseResult _  (At.Done    _  r) = validate r

handleParseFail :: Text -> Text -> Either T.ErrString [T.Issue]
handleParseFail input rest = Left msg
    where n   = length . Tx.lines $ input
          m   = length . Tx.lines $ rest
          msg = unwords [ "Parse failure at line "
                        , show (n - m + 1) <> " : "
                        , Tx.unpack . Tx.takeWhile (not . Ch.isSpace) $ rest
                        , "..."
                        ]

-- =============================================================== --
-- Error message construction

parseError :: String -> T.ErrString -> T.ErrString
parseError fp err = "Unable to parse references file " <> fp <> "!\n" <> err

validationError :: Dict -> T.ErrString -> T.ErrString
validationError d err = maybe noJournal go . lookup "journal" $ d
    where noJournal = "Fields provided without preceding <journal:> header!"
          go x = concat [ "Unable read journal reference for " <> Tx.unpack  x
                        , "\n" <> err
                        ]

resetsError :: T.ErrString
resetsError = intercalate "\n" hs
    where hs = [ "Missing or invalid <resets> value!"
               , "Use 'true' if issue numbers reset to 1 each year."
               , "Use 'false' if issue numbers increase each year."
               ]

frequencyError :: T.ErrString
frequencyError = intercalate "\n" hs
    where hs = [ "Missing or invalid <frequency> value!"
               , "Use 'weekly' if there are always 52 issues every year."
               , "Use 'weekly-last' if the last issue of the year is dropped."
               , "Use 'weekly-first' if th first issue of the year is dropped."
               , "Use 'monthly' if there are 12 issues every year."
               ]

-- =============================================================== --
-- Construction and validation of issues and journals

---------------------------------------------------------------------
-- Reference construction and validation after file parsing

validate :: [Dict] -> Either T.ErrString [T.Issue]
validate ds = mapM readRef ds >>= checkForDuplicates

checkForDuplicates :: [T.Issue] -> Either T.ErrString [T.Issue]
-- ^Journal entries are all keyed by their abbreviations. So, the
-- journal abbreviations must be unique. However, this function also
-- checks to make sure the journal names are also unique.
checkForDuplicates xs
    | gNames && gAbbrs = pure xs
    | gNames           = Left "References have repeated journal abbreviations!"
    | otherwise        = Left "References have repeated journal names!"
    where gNames = ys == nub ys
          gAbbrs = zs == nub zs
          ys     = map (T.name . T.journal) xs
          zs     = map (T.abbr . T.journal) xs

readRef :: Dict -> Either T.ErrString T.Issue
readRef d = bimap (validationError d) id ref
    where ref = T.Issue <$> readDate    d
                        <*> readInt     d "volume"
                        <*> readInt     d "issue"
                        <*> readJournal d

---------------------------------------------------------------------
-- General helpers

readString :: Dict -> Text -> Either T.ErrString Text
readString d k = maybe err go . lookup k $ d
    where go x = checkString x
          err  = Left $ "Record lacks a <" <> Tx.unpack k <> "> field!"

readInt :: Dict -> Text -> Either T.ErrString Int
readInt d k = readString d k >>= go
    where go   = maybe err pure . C.readMaybeTxt
          err  = Left $ "Record requires an integer value for the <"
                        <> Tx.unpack k <> "> field!"

checkString :: Text -> Either T.ErrString Text
checkString x
    | allValid  = pure x
    | otherwise = Left errMsg
    where allValid = Tx.all ( \ c -> Ch.isAlphaNum c || c == '_' || c == ' ' ) x
          errMsg   = "The field value '" <> Tx.unpack x
                     <> "' contains invalid characters!"

toMonth :: Text -> Maybe Int
toMonth "january"  = pure 1
toMonth "february" = pure 2
toMonth "march"    = pure 3
toMonth "april"    = pure 4
toMonth "may"      = pure 5
toMonth "june"     = pure 6
toMonth "july"     = pure 7
toMonth "august"   = pure 8
toMonth "september"= pure 9
toMonth "october"  = pure 10
toMonth "november" = pure 11
toMonth "december" = pure 12
toMonth x = C.readMaybeTxt x >>= go
    where go n | n > 0 && n < 13 = pure n
               | otherwise       = Nothing

prepString :: Text -> Text
prepString = Tx.map Ch.toLower . Tx.strip

---------------------------------------------------------------------
-- Journal construction and validation

readJournal :: Dict -> Either T.ErrString T.Journal
readJournal d = do
    (j, k) <- readJournalHeader d
    checkString j
    checkString k
    T.Journal k j <$> readString    d "pubmed"
                  <*> readFrequency d
                  <*> readResets    d

readFrequency :: Dict -> Either T.ErrString T.Frequency
readFrequency = maybe (Left frequencyError) go . lookup "frequency"
    where go x = case prepString x of
                      "weekly"       -> pure T.Weekly
                      "weekly-first" -> pure T.WeeklyFirst
                      "weekly-last"  -> pure T.WeeklyLast
                      "monthly"      -> pure T.Monthly
                      _              -> Left frequencyError

readDate :: Dict -> Either T.ErrString Tm.Day
readDate dict = do
    d <- readInt dict "day"
    y <- fromIntegral <$> readInt dict "year"
    m <- readMonth    dict
    pure $ Tm.fromGregorian y m d

readMonth :: Dict -> Either T.ErrString Int
readMonth dict = maybe err pure $ lookup "month" dict >>= toMonth . prepString
    where err  = Left "Missing or invalid <month> field!"

readResets :: Dict -> Either T.ErrString Bool
readResets = maybe (Left resetsError) go . lookup "resets"
    where go x = case prepString $ x of
                       "true"  -> pure True
                       "false" -> pure False
                       _       -> Left resetsError

readJournalHeader :: Dict -> Either T.ErrString (Text, Text)
readJournalHeader =  maybe (Left "") go . lookup "journal"
    where go x  = case break (== '/') . Tx.unpack $ x of
                       ([]  , _      ) -> Left " Missing journal name!"
                       (_   ,'/':[]  ) -> Left " Missing journal abbreviation!"
                       (name,'/':abbr) -> pure (Tx.pack name, Tx.pack abbr)
                       (_   , _      ) -> Left " Missing journal abbreviation!"

-- =============================================================== --
-- Dict parser for parsing the file prior to reference construction

---------------------------------------------------------------------
-- Helpers

field :: At.Parser (Text, Text)
field = At.choice $ map keyValuePair [ "day"
                                     , "frequency"
                                     , "issue"
                                     , "month"
                                     , "pubmed"
                                     , "resets"
                                     , "volume"
                                     , "year"
                                     ]

comment :: At.Parser ()
comment = At.char '#' *> At.takeTill At.isEndOfLine *> At.skipSpace

comments :: At.Parser ()
comments = At.skipSpace *> many comment *> pure ()

validValue :: At.Parser Text
validValue = fmap Tx.pack . some . At.satisfy $ At.notInClass ":#\n\r\t"

keyValuePair :: Text -> At.Parser (Text, Text)
keyValuePair key = do
    comments
    k <- At.string key
    comments
    At.char ':'
    comments
    v <- validValue
    comment <|> At.endOfLine
    pure (k,v)

---------------------------------------------------------------------
-- Reference Dict parsers

refDicts :: At.Parser [Dict]
refDicts = do
    comments
    ds <- many refDict
    comments
    At.endOfInput
    pure ds

refDict :: At.Parser Dict
refDict = do
    jh <- keyValuePair "journal"
    fs <- some field
    comments
    pure $ jh : fs