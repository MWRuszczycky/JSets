{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Config
    ( parseConfig
      -- Configurators
    , configAddToQuery
    , configAddSkipDay
    , configArguments
    , configDelay
    , configESumChunkSize
    , configFormat
    , configIntQuery
    , configKey
    , configMaxResults
    , configOutputPath
    , configPattern
    , configPageQuery
    , configStartDay
    ) where

import qualified Data.Text            as Tx
import qualified Data.Time            as Tm
import qualified Data.Attoparsec.Text as At
import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Model.Core.Dates     as D
import qualified Model.Parsers.Core   as P
import           Data.Char                   ( isAlphaNum, toLower
                                             , isDigit             )
import           Data.Text                   ( Text                )
import           Data.List                   ( intercalate         )
import           Text.Read                   ( readMaybe           )
import           Control.Applicative         ( (<|>), many, some   )
import           Control.Monad               ( guard               )
import           Control.Monad.Except        ( liftEither
                                             , throwError          )

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
parseParameter = readConfigParam <$> keyValuePair

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
-- Readers for reference key-value pairs

readRef :: [KeyValPair] -> T.ConfigStep
readRef ps = T.ConfigInit $ \ c -> ref >>= go c
    where go c r = let rs = T.cReferences c
                   in  pure $ c { T.cReferences = rs <> [r] }
          ref = liftEither . checkRef ps $ T.Issue <$> readDate    ps
                                                   <*> readInt     ps "volume"
                                                   <*> readInt     ps "issue"
                                                   <*> readJournal ps

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
                        <*> (maybe (pure True) pure . lookupFlag ps) "followed"

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
                       "mid-monthly"  -> pure T.MidMonthly
                       "end-monthly"  -> pure T.EndMonthly
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
readResets ps = maybe (Left resetsError) pure . lookupFlag ps $ "resets"

---------------------------------------------------------------------
-- Reference read validation and error handling

checkRef :: [(KeyValPair)] -> Either T.ErrString T.Issue
            -> Either T.ErrString T.Issue
checkRef ps (Left  err) = Left $ readRefError ps err
checkRef ps (Right iss)
    | isMidMonthly && isEarly = Left $ readRefError ps errMsg
    | otherwise               = pure iss
    where isMidMonthly = (== T.MidMonthly) . T.freq . T.journal $ iss
          isEarly      = (< 15) . T.day . T.date $ iss
          errMsg       = unwords [ "mid-monthly references must be published"
                                 , "after the 14-th of the month."
                                 ]

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
               , "Use 'monthly' if monthly towards the beginning of the month."
               , "Use 'mid-monthly' if monthly towards the end of the month."
               , "Use 'once-monthly' if monthly at irregural intervals."
               , "Use 'semimonthly' if there are 24 issues every year."
               , "Use a number n if issues are published every n weeks."
               ]

flagError :: Text -> T.ErrString
flagError p = "Invalid boolean flag for "
               <> Tx.unpack p
               <> ", use 'yes/no/true/false'."

---------------------------------------------------------------------
-- General helpers for reading parameters

readString :: RefKeyValPairs -> Text -> Either T.ErrString Text
readString ps k = maybe err go . lookup k $ ps
    where go x = checkString x
          err  = Left $ "Record lacks a <" <> Tx.unpack k <> "> field!"

readInt :: RefKeyValPairs -> Text -> Either T.ErrString Int
readInt ps k = readString ps k >>= go
    where go   = maybe err pure . C.readMaybeTxt
          err  = Left $ "Record requires an integer value for the <"
                        <> Tx.unpack k <> "> field!"

readFlag :: Text -> Maybe Bool
readFlag = go . prepString
    where go "yes"   = Just True
          go "true"  = Just True
          go "no"    = Just False
          go "false" = Just False
          go _       = Nothing

lookupFlag :: RefKeyValPairs -> Text -> Maybe Bool
lookupFlag ps k = lookup k ps >>= readFlag

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

-- ================================================================== 
-- Reading parameters to generate configurators

-- ------------------------------------------------------------------ 
-- Reading configuration parameters from file

readConfigParam :: KeyValPair -> T.ConfigStep
readConfigParam ("by-date", x) =
    T.ConfigGen $ \ c -> maybe ( throwError $ flagError "by-date" )
                         ( \ b -> pure $ c { T.cYearlyByDate = b } )
                         . readFlag $ x
readConfigParam ("delay", d) =
    T.ConfigGen . configDelay . Tx.unpack $ d
readConfigParam ("email", e) =
    T.ConfigGen $ \ c -> pure $ c { T.cEmail = Just e }
readConfigParam ("max-docsum", s) =
    T.ConfigGen . configESumChunkSize . Tx.unpack $ s
readConfigParam ("max-results", m) =
    T.ConfigGen . configMaxResults . Tx.unpack $ m
readConfigParam ("no-sort", x) =
    T.ConfigGen $ \ c -> maybe ( throwError $ flagError "no-sort" )
                         ( \ b -> pure $ c { T.cSortJSets = not b } )
                         . readFlag $ x
readConfigParam ("user",  u) =
    T.ConfigGen $ \ c -> pure $ c { T.cUser  = Just u }
readConfigParam ("pattern", p) =
    T.ConfigGen . configPattern . Tx.unpack $ p
readConfigParam ("presenter", p) =
    T.ConfigGen $ \ c -> let ps = T.cPresenters c
                         in  pure $ c { T.cPresenters = ps <> [p] }
readConfigParam ("skip-day", d) =
    T.ConfigGen . configAddSkipDay . Tx.unpack $ d
readConfigParam (p,_) = T.ConfigWarn warning
    where warning = "Unrecognized parameter: " <> p <> " (ignored)"

-- ------------------------------------------------------------------ 
-- The specialized configurators

configAddToQuery :: T.QueryTerm -> T.Configurator
configAddToQuery q config = pure $ config { T.cQuery = q:qs}
    where qs = T.cQuery config

configAddSkipDay :: String -> T.Configurator
configAddSkipDay dayStr config = maybe err go . readDay $ dayStr
    where yr        = fromIntegral . T.year . T.cDate $ config
          readDay x = D.readDate x <|> D.readMonthDay yr x
          ds        = T.cSkipDays config
          go d      = pure $ config { T.cSkipDays = ds <> [d] }
          err       = throwError . unwords $
                          [ "Invalid date:", dayStr <> ", dates should be"
                          , "specified as YYYY-MM-DD or MM-DD."
                          ]

configArguments :: [String] -> T.Configurator
configArguments args config = pure $ config { T.cArguments = args }

configDelay :: String -> T.Configurator
configDelay delay config
    | d < 1     = throwError $ "Delay time must be a positive integer."
    | otherwise = pure $ config { T.cDelay = d }
    where d = maybe 0 id . readMaybe $ delay

configESumChunkSize :: String -> T.Configurator
configESumChunkSize size config
    | s < 1     = throwError $ "The ESummary size must be a positive integer."
    | otherwise = pure $ config { T.cESumChunkSize = s }
    where s = maybe 0 id . readMaybe $ size

configFormat :: String -> T.Configurator
-- ^This should be a part of a general configuration step so that it
-- takes precedence over the format set by the output path extension. 
configFormat arg config = maybe err go . C.readFormat $ arg
    where go x = pure $ config { T.cFormat = x }
          err  = throwError $ "Unrecognized format " <> arg

configIntQuery :: (Int -> T.QueryTerm) -> String -> T.Configurator
configIntQuery q x config = go x >>= flip configAddToQuery config . q
    where go x = maybe err chk . readMaybe $ x
          err  = throwError $ "Argument " <> x <> " is not an unsigned integer!"
          chk n | n < 0     = err
                | otherwise = pure n

configKey :: String -> T.Configurator
configKey key config
    | n < 1     = throwError $ "Key must be a positive integer."
    | otherwise = pure $ config { T.cJSetKey = Just n }
    where n = maybe 0 id . readMaybe $ key

configMaxResults :: String -> T.Configurator
configMaxResults maxresults config
    | n < 1     = throwError $ "Maximum results must be a positive integer."
    | otherwise = pure $ config { T.cMaxResults = n }
    where n = maybe 0 id . readMaybe $ maxresults

configOutputPath :: FilePath -> T.Configurator
-- ^Configure both the output path as well as the format. This should
-- be an initial configuration step so that the format provided by
-- the --fmt option will take precedence if used.
configOutputPath fp config = pure $
    config { T.cOutputPath = Just fp
           , T.cFormat     = maybe (T.cFormat config) id
                             . C.readFormat . C.extension $ fp }

configPattern :: String -> T.Configurator
configPattern [] _ =
    throwError "A meeting pattern must have at least one character."
configPattern xs@(x:_) config = pure $ config { T.cMeetPattern = map go xs }
    where go c = c == x

configPageQuery :: String -> T.Configurator
configPageQuery xs config = go pgno >>= flip configAddToQuery config
    where (rd,rp) = span isDigit . reverse $ xs
          pgno    = T.PageNo (reverse rp) <$> (readMaybe . reverse $ rd)
          go      = maybe err (pure . T.PageQry)
          err     = throwError $ "Invalid page number " <> xs <> "!"

configStartDay :: String -> T.Configurator
configStartDay dayStr config = maybe err go . readDay $ dayStr
    where go d      = pure $ config { T.cStartDay = pure d }
          yr        = fromIntegral . T.year . T.cDate $ config
          readDay x = D.readDate x <|> D.readMonthDay yr x
          err       = throwError . unwords $
                       [ "Invalid date:", dayStr <> ", the start day should be"
                       , "specified as YYYY-MM-DD or MM-DD."
                       ]
