{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Config
    ( parseConfig
    ) where

import qualified Data.Text            as Tx
import qualified Data.Time            as Tm
import qualified Data.Attoparsec.Text as At
import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Model.Parsers.Core   as P
import           Data.Char                   ( isAlphaNum, toLower )
import           Data.Text                   ( Text                )
import           Data.List                   ( intercalate         )
import           Data.Bifunctor              ( bimap               )
import           Control.Applicative         ( (<|>), many, some   )
import           Control.Monad               ( guard               )
import           Control.Monad.Except        ( liftEither          )

-- =============================================================== --
-- Parsing a configuration file to configuration steps

parseConfig :: Text -> Either T.ErrString [T.ConfigStep]
parseConfig txt = handleResult txt . At.parse configFile $ txt

handleResult :: Text -> At.Result [T.ConfigStep]
                -> Either T.ErrString [T.ConfigStep]
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

-- =============================================================== --
-- Local helper types

-- |Generic key-value pair
type KeyValPair     = (Text,Text)

-- |List of key-value pairs that describe an reference issue
type RefKeyValPairs = [KeyValPair]

-- =============================================================== --
-- Parsers: Configuration files are parsed to lists of (Text,Text)
-- key-value pairs. These are then read to generate the ConfigSteps,
-- which can then be used to modify the configuration (Config).
-- Configuration parameters are expected to precede the configured
-- references, because each reference begins with the journal name
-- and abbreviation specification but has no terminator.

configFile :: At.Parser [T.ConfigStep]
configFile = do
    ps <- many $ parseParameter
    rs <- many $ parseReference
    P.comments
    At.endOfInput
    pure $ ps <> rs

parseParameter :: At.Parser T.ConfigStep
parseParameter = readParam <$> keyValuePair

parseReference :: At.Parser T.ConfigStep
parseReference = readRef <$> refKeyValuePairs

---------------------------------------------------------------------
-- Component key-value pair parsers

validKey :: At.Parser Text
validKey = At.takeWhile1 $ \ c -> isAlphaNum c || c == '-' || c == '_'

validValue :: At.Parser Text
validValue = fmap Tx.strip . At.takeWhile1 . At.notInClass $ ":#\n\r\t"

refKeyValuePairs :: At.Parser RefKeyValPairs
refKeyValuePairs = do
    P.comments *> At.string "journal" *> P.comments *> At.char ':' *> P.comments
    jh <- (,) "journal" <$> validValue
    P.comment <|> At.endOfLine
    fs <- some keyValuePair
    pure $ jh : fs

keyValuePair :: At.Parser KeyValPair
keyValuePair = do
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
-- Readers for parameter and reference key-value pairs

readRef :: [KeyValPair] -> T.ConfigStep
readRef ps = T.ConfigInit $ \ c -> ref >>= go c
    where go c r = let rs = T.cReferences c
                   in  pure $ c { T.cReferences = rs <> [r] }
          ref = liftEither . bimap (readRefError ps ) id $
                    T.Issue <$> readDate    ps
                            <*> readInt     ps "volume"
                            <*> readInt     ps "issue"
                            <*> readJournal ps

readParam :: KeyValPair -> T.ConfigStep
readParam ("user",  u) = T.ConfigGen $ \ c -> pure $ c { T.cUser  = Just u }
readParam ("email", e) = T.ConfigGen $ \ c -> pure $ c { T.cEmail = Just e }
readParam (p,       _) = T.ConfigWarn warning
    where warning = "Unrecognized parameter: " <> p <> " (ignored)"

---------------------------------------------------------------------
-- Components for reading the key-value pairs for references

readJournal :: RefKeyValPairs -> Either T.ErrString T.Journal
readJournal ps = do
    (name, abbr) <- readJournalHeader ps
    checkString name
    checkString abbr
    T.Journal abbr name <$> readString     ps "pubmed"
                        <*> readFrequency  ps
                        <*> readResets     ps
                        <*> readInt        ps "mincount"
                        <*> (maybe (pure True) pure . readFlag ps) "followed"

readJournalHeader :: RefKeyValPairs -> Either T.ErrString (Text, Text)
readJournalHeader =  maybe (Left "") go . lookup "journal"
    where go x  = case break (== '/') . Tx.unpack $ x of
                       ([]  , _      ) -> Left " Missing journal name!"
                       (_   ,'/':[]  ) -> Left " Missing journal abbreviation!"
                       (name,'/':abbr) -> pure (Tx.pack name, Tx.pack abbr)
                       (_   , _      ) -> Left " Missing journal abbreviation!"

readFrequency :: RefKeyValPairs -> Either T.ErrString T.Frequency
readFrequency = maybe (Left frequencyError) go . lookup "frequency"
    where chk n | n > 0     = pure . T.EveryNWeeks $ n
                | otherwise = Left frequencyError
          go x  = case prepString x of
                       "weekly"       -> pure $ T.EveryNWeeks 1
                       "weekly-first" -> pure T.WeeklyFirst
                       "weekly-last"  -> pure T.WeeklyLast
                       "monthly"      -> pure T.Monthly
                       "once-monthly" -> pure T.OnceMonthly
                       "semimonthly"  -> pure T.SemiMonthly
                       nstr           -> maybe ( Left frequencyError )
                                               chk . C.readMaybeTxt $ nstr

readDate :: RefKeyValPairs -> Either T.ErrString Tm.Day
readDate ps = do
    d <- readInt   ps "day"
    y <- fromIntegral <$> readInt ps "year"
    m <- readMonth ps
    pure $ Tm.fromGregorian y m d

readMonth :: RefKeyValPairs -> Either T.ErrString Int
readMonth ps = maybe err pure $ lookup "month" ps >>= toMonth . prepString
    where err  = Left "Missing or invalid <month> field!"

readResets :: RefKeyValPairs -> Either T.ErrString Bool
readResets ps = maybe (Left resetsError) pure . readFlag ps $ "resets"

---------------------------------------------------------------------
-- Reference read validation and error handling

readRefError :: [(KeyValPair)] -> T.ErrString -> T.ErrString
readRefError ps err = maybe noJournal go . lookup "journal" $ ps
    where noJournal = "Fields provided without preceding <journal> header!"
          go x      = concat [ "Unable to read journal reference for "
                             , Tx.unpack x, ":\n" <> err
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
               , "Use 'weekly-first' if the first issue of the year is dropped."
               , "Use 'monthly' if there are 12 issues every year."
               , "Use 'semimonthly' if there are 24 issues every year."
               , "Use a number n if issues are published every n weeks."
               ]

---------------------------------------------------------------------
-- General helpers for reading journal issue references

readString :: RefKeyValPairs -> Text -> Either T.ErrString Text
readString ps k = maybe err go . lookup k $ ps
    where go x = checkString x
          err  = Left $ "Record lacks a <" <> Tx.unpack k <> "> field!"

readInt :: RefKeyValPairs -> Text -> Either T.ErrString Int
readInt ps k = readString ps k >>= go
    where go   = maybe err pure . C.readMaybeTxt
          err  = Left $ "Record requires an integer value for the <"
                        <> Tx.unpack k <> "> field!"

readFlag :: RefKeyValPairs -> Text -> Maybe Bool
readFlag ps k = lookup k ps >>= go . prepString
    where go "yes"   = Just True
          go "true"  = Just True
          go "no"    = Just False
          go "false" = Just False
          go _       = Nothing

checkString :: Text -> Either T.ErrString Text
checkString x
    | allValid  = pure x
    | otherwise = Left errMsg
    where allValid = Tx.all ( \ c -> isAlphaNum c || c == '_' || c == ' ' ) x
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
prepString = Tx.map toLower . Tx.strip
