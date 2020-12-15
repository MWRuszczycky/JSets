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
import           Data.Char                   ( isAlphaNum        )
import           Data.Text                   ( Text              )
import           Data.List                   ( nub, intercalate  )
import           Data.Bifunctor              ( bimap             )
import           Control.Applicative         ( (<|>), many, some )
import           Control.Monad               ( guard             )

-- TODO : Refactor so there is only one function that parses the
-- configuration file directly to ConfigSteps.
-- The checking for duplicate references should be done in the
-- finalization steps of the configuration not here.

-- =============================================================== --
-- Configuration file parsers and readers

-- Configuration file parsing takes place in two stages.
--
-- 1. The raw text file is parsed to Parameter key-value Text pairs.
--    Configuration parameters and reference issue parameters are
--    each parsed separately to generate a ConfigFile value.
--    This is all done with the parseConfig function.
--
-- 2. The ConfigFile key-value Text pairs (type Parameter) can then
--    be converted to a Config on which the AppMonad is based as
--    using the readConfig function.

-- =============================================================== --
-- Parsing a configuration file to lists of Parameter key-value pairs

parseConfig :: Text -> Either T.ErrString T.ConfigFile
parseConfig txt = handleResult txt . At.parse configFile $ txt

handleResult :: Text -> At.Result T.ConfigFile
                -> Either T.ErrString T.ConfigFile
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

configFile :: At.Parser T.ConfigFile
configFile = T.ConfigFile <$> configParameters <*> refDicts

configParameters :: At.Parser [T.Parameter]
configParameters = P.comments *> many parameter

---------------------------------------------------------------------
-- Parsing journal reference issues

refDicts :: At.Parser [T.RefParameters]
refDicts = do
    P.comments
    ds <- many refParameters
    P.comments
    At.endOfInput
    pure ds

refParameters :: At.Parser T.RefParameters
refParameters = do
    P.comments *> At.string "journal" *> P.comments *> At.char ':' *> P.comments
    jh <- (,) "journal" <$> validValue
    P.comment <|> At.endOfLine
    fs <- some parameter
    pure $ jh : fs

---------------------------------------------------------------------
-- Parse helpers

validValue :: At.Parser Text
validValue = fmap Tx.strip . At.takeWhile1 . At.notInClass $ ":#\n\r\t"

validKey :: At.Parser Text
validKey = At.takeWhile1 $ \ c -> isAlphaNum c || c == '-' || c == '_'

parameter :: At.Parser T.Parameter
parameter = do
    P.comments
    k <- validKey
    guard $ k /= "journal"
    P.comments
    At.char ':'
    P.comments
    v <- validValue
    P.comment <|> At.endOfLine
    pure (k,v)

-- =============================================================== --
-- Reading configuration files and journal issue references
-- This is where the raw Text Parameters representing the parsed
-- configuration file are read and checked as configuration values.

readConfig :: T.ConfigFile -> Either T.ErrString [T.ConfigStep]
readConfig (T.ConfigFile configParams configRefs) = do
    refConfigStep <- readRefs configRefs
    pure $ refConfigStep : map readParam configParams

readRefs :: [T.RefParameters] -> Either T.ErrString T.ConfigStep
readRefs ds = mapM readRef ds >>= checkForDuplicates >>= go
    where go rs = pure . T.ConfigInit $ \ c -> pure $ c { T.cReferences = rs }

readParam :: (Text,Text) -> T.ConfigStep
readParam ("user",  u) = T.ConfigGen $ \ c -> pure $ c { T.cUser  = Just u }
readParam ("email", e) = T.ConfigGen $ \ c -> pure $ c { T.cEmail = Just e }
readParam (p,       _) = T.ConfigWarn warning
    where warning = "Unrecognized parameter: " <> p <> " (ignored)"

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

readError :: T.RefParameters -> T.ErrString -> T.ErrString
readError ps err = maybe noJournal go . lookup "journal" $ ps
    where noJournal = "Fields provided without preceding <journal> header!"
          go x = concat [ "Unable read journal reference for " <> Tx.unpack  x
                        , "\n" <> err
                        ]

resetsError :: T.ErrString
resetsError = intercalate "\n" hs
    where hs = [ "Missing or invalid <resets> value!"
               , "Use 'true' or 'yes' if issue numbers reset to 1 each year."
               , "Use 'false' or 'no' if issue numbers increase each year."
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

readString :: T.RefParameters -> Text -> Either T.ErrString Text
readString ps k = maybe err go . lookup k $ ps
    where go x = checkString x
          err  = Left $ "Record lacks a <" <> Tx.unpack k <> "> field!"

readInt :: T.RefParameters -> Text -> Either T.ErrString Int
readInt ps k = readString ps k >>= go
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

readRef :: T.RefParameters -> Either T.ErrString T.Issue
readRef ps = bimap (readError ps) id ref
    where ref = T.Issue <$> readDate    ps
                        <*> readInt     ps "volume"
                        <*> readInt     ps "issue"
                        <*> readJournal ps

readJournal :: T.RefParameters -> Either T.ErrString T.Journal
readJournal ps = do
    (j, k) <- readJournalHeader ps
    checkString j
    checkString k
    T.Journal k j <$> readString    ps "pubmed"
                  <*> readFrequency ps
                  <*> readResets    ps
                  <*> readInt       ps "mincount"

readFrequency :: T.RefParameters -> Either T.ErrString T.Frequency
readFrequency = maybe (Left frequencyError) go . lookup "frequency"
    where go x = case prepString x of
                      "weekly"       -> pure T.Weekly
                      "weekly-first" -> pure T.WeeklyFirst
                      "weekly-last"  -> pure T.WeeklyLast
                      "monthly"      -> pure T.Monthly
                      _              -> Left frequencyError

readDate :: T.RefParameters -> Either T.ErrString Tm.Day
readDate ps = do
    d <- readInt   ps "day"
    y <- fromIntegral <$> readInt ps "year"
    m <- readMonth ps
    pure $ Tm.fromGregorian y m d

readMonth :: T.RefParameters -> Either T.ErrString Int
readMonth ps = maybe err pure $ lookup "month" ps >>= toMonth . prepString
    where err  = Left "Missing or invalid <month> field!"

readResets :: T.RefParameters -> Either T.ErrString Bool
readResets = maybe (Left resetsError) go . lookup "resets"
    where go x = case prepString x of
                      "true"  -> pure True
                      "yes"   -> pure True
                      "false" -> pure False
                      "no"    -> pure False
                      _       -> Left resetsError

readJournalHeader :: T.RefParameters -> Either T.ErrString (Text, Text)
readJournalHeader =  maybe (Left "") go . lookup "journal"
    where go x  = case break (== '/') . Tx.unpack $ x of
                       ([]  , _      ) -> Left " Missing journal name!"
                       (_   ,'/':[]  ) -> Left " Missing journal abbreviation!"
                       (name,'/':abbr) -> pure (Tx.pack name, Tx.pack abbr)
                       (_   , _      ) -> Left " Missing journal abbreviation!"
