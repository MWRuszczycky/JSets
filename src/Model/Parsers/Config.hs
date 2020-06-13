{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.Config
    ( parseConfig
    , readConfig
    ) where

import qualified Data.Text            as Tx
import qualified Data.Time            as Tm
import qualified Data.Char            as Ch
import qualified Data.Attoparsec.Text as At
import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Model.Parsers.Core   as P
import           Model.Core.Types            ( Dict, ConfigFile  (..) )
import           Data.Text                   ( Text                   )
import           Data.List                   ( nub, intercalate       )
import           Data.Bifunctor              ( bimap                  )
import           Control.Applicative         ( (<|>), many, some      )
import           Control.Monad               ( foldM                  )

-- =============================================================== --
-- Configuration file parsers and readers

-- Configuration file parsing takes place in two stages.
--
-- 1. The raw text file is parsed to structured data in the form of
--    of Text -> Text dictionaries pairing a configuraton parameter
--    with a configured value. The general configuration parameters
--    are parsed into a single such dictionary. Each following
--    journal reference issue dictionary is then parsed into its own
--    Text -> Text dictionary. These dictionaries of type Dict are
--    then packaged into a ConfigFile value.
--    This parsing is handeled by parseConfig.
--
-- 2. The parsed, structured data representing the configuration file
--    as a value of type ConfigFile is then read into reference
--    reference issues and the final configuration value.
--    This reading is handeled by readConfig.

-- =============================================================== --
-- Parsing a configuration file to a configuration dictionary before
-- reading to the file configuration.

parseConfig :: Text -> Either T.ErrString ConfigFile
parseConfig txt = handleResult txt . At.parse configFile $ txt

handleResult :: Text -> At.Result ConfigFile -> Either T.ErrString ConfigFile
handleResult xs (At.Fail ys _  _) = handleFail xs ys
handleResult xs (At.Partial go  ) = handleResult xs . go $ Tx.empty
handleResult _  (At.Done    _  r) = pure r

handleFail :: Text -> Text -> Either T.ErrString a
handleFail input rest = Left msg
    where n     = length . Tx.lines $ input
          m     = length . Tx.lines $ rest
          atEnd = flip elem ['\n', '\r']
          msg = unwords [ "Parse failure at or after line"
                        , show (n - m + 1) <> " : ..."
                        , Tx.unpack . Tx.takeWhile (not . atEnd) $ rest
                        , "..."
                        ]

---------------------------------------------------------------------
-- Configuration dicts

configFile :: At.Parser ConfigFile
configFile = ConfigFile <$> configDict <*> refDicts

configDict :: At.Parser Dict
configDict = P.comments *> many configField

configField :: At.Parser (Text, Text)
configField = At.choice $ map keyValuePair [ "email"
                                           , "user"
                                           , "nickname"
                                           ]

---------------------------------------------------------------------
-- Parsing reference dictionaries

refDicts :: At.Parser [Dict]
refDicts = do
    P.comments
    ds <- many refDict
    P.comments
    At.endOfInput
    pure ds

refDict :: At.Parser Dict
refDict = do
    jh <- keyValuePair "journal"
    fs <- some refField
    P.comments
    pure $ jh : fs

refField :: At.Parser (Text, Text)
refField = At.choice $ map keyValuePair [ "day"
                                        , "frequency"
                                        , "issue"
                                        , "month"
                                        , "pubmed"
                                        , "resets"
                                        , "volume"
                                        , "year"
                                        ]

---------------------------------------------------------------------
-- Parse helpers

validValue :: At.Parser Text
validValue = fmap Tx.pack . some . At.satisfy $ At.notInClass ":#\n\r\t"

keyValuePair :: Text -> At.Parser (Text, Text)
keyValuePair key = do
    P.comments
    k <- At.string key
    P.comments
    At.char ':'
    P.comments
    v <- Tx.strip <$> validValue
    P.comment <|> At.endOfLine
    pure (k,v)

-- =============================================================== --
-- Reading configuration files and journal issue references

readConfig :: T.Config -> T.ConfigFile -> Either T.ErrString T.Config
readConfig config (T.ConfigFile hdrs dicts) = do
    config' <- foldM readHeader config hdrs
    refs    <- readRefs dicts
    pure $ config' { T.cReferences = refs }

readRefs :: [Dict] -> Either T.ErrString [T.Issue]
readRefs ds = mapM readRef ds >>= checkForDuplicates

readHeader :: T.Config -> (Text,Text) -> Either T.ErrString T.Config
-- ^Assign configuration data from header information. Note that
-- configuration data can be overridden by command line options.
readHeader c ("user", u )    = pure $ c { T.cUser  = T.cUser  c <|> pure u }
readHeader c ("email", e)    = pure $ c { T.cEmail = T.cEmail c <|> pure e }
readHeader c ("nickname", n) = pure $ c { T.cNick  = T.cNick  c <|> pure n }
readHeader c _               = pure c

---------------------------------------------------------------------
-- Read validation and error handling

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

readError :: Dict -> T.ErrString -> T.ErrString
readError d err = maybe noJournal go . lookup "journal" $ d
    where noJournal = "Fields provided without preceding <journal> header!"
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

---------------------------------------------------------------------
-- General helpers for reading journal issue references

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
-- Reading journal issue references dictionaries

readRef :: Dict -> Either T.ErrString T.Issue
readRef d = bimap (readError d) id ref
    where ref = T.Issue <$> readDate    d
                        <*> readInt     d "volume"
                        <*> readInt     d "issue"
                        <*> readJournal d

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
