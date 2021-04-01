{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Model.Parsers.Citations
    ( parseCitations
    , parsePMIDs
    , parseCitationRIS
    ) where

import qualified Data.Attoparsec.Text as At
import qualified Model.Core.Core      as C
import qualified Model.Parsers.JSON   as JS
import qualified Data.Map.Strict      as Map
import qualified Model.Parsers.Core   as P
import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import           Control.Monad                ( guard, replicateM   )
import           Control.Applicative          ( (<|>), many, some   )
import           Data.Bifunctor               ( bimap               )
import           Data.Char                    ( isAlphaNum, isSpace )
import           Data.List                    ( foldl'              )
import           Data.Text                    ( Text                )
import           Data.Time                    ( Day, fromGregorian  )
import           Model.Journals               ( fakePMID            )

-- =============================================================== --
-- Parsing PubMed Entrez ESearch results

parsePMIDs :: Text -> Either T.ErrString [T.PMID]
parsePMIDs json = JS.parse json >>= maybe err pure . go
    where err = Left "Unable to read idlist from PMID request!"
          go  = JS.lookupListWith [ "esearchresult", "idlist" ] JS.str

-- =============================================================== --
-- Parsing PubMed Entrez ESummary results

parseCitations :: [T.PMID] -> Text -> Either T.ErrString ([T.PMID], T.Citations)
-- ^Parse eSummary json from PubMed to construct citations. If any
-- pmids are indexed in the json but do not have corresponding
-- document summaries, these pmids are also returned. These represent
-- requested PMIDs that were not found (e.g., if incorrect).
parseCitations pmids txt = do
    json  <- JS.parse txt
    let (cs, missing) = C.splitMaybe . map (getCitation json) $ pmids
    pure (missing, Map.fromList cs)

---------------------------------------------------------------------
-- Components

getCitation :: JS.JSON -> T.PMID -> (T.PMID, Maybe T.Citation)
getCitation json pmid = (pmid, c)
    where c = do doc <- JS.lookupWith [ "result", pmid ] pure json
                 T.Citation <$> JS.lookupWith [ "title" ] JS.str doc
                            <*> getAuthors doc
                            <*> getIssue   doc
                            <*> getPages   doc
                            <*> getDoi     doc
                            <*> pure       pmid

getAuthors :: JS.JSON -> Maybe [Text]
getAuthors json = JS.lookupListWith [ "authors" ] go json
    where go = JS.lookupWith [ "name" ] JS.str

getIssue :: JS.JSON -> Maybe T.Issue
getIssue json = T.Issue <$> getDate json
                        <*> getInt [ "volume" ] json
                        <*> getInt [ "issue"  ] json
                        <*> getJournal json

getDate :: JS.JSON -> Maybe Day
getDate json = pubDate <|> epubDate
    where pubDate   = JS.lookupWith [ "pubdate"  ] JS.str json >>= checkDate
          epubDate  = JS.lookupWith [ "epubdate" ] JS.str json >>= checkDate
          checkDate = either (const Nothing) pure . At.parseOnly parseDate

getInt :: [Text] -> JS.JSON -> Maybe Int
getInt keys json = mbInt <|> Just 0
    where mbInt = JS.lookupWith keys JS.str json >>= C.readMaybeTxt

getJournal :: JS.JSON -> Maybe T.Journal
getJournal json = JS.lookupWith [ "source" ] JS.str json >>= pure . go
    where go j = T.Journal j j j T.UnknownFreq True 20 False

getDoi :: JS.JSON -> Maybe Text
getDoi json = JS.lookupWith [ "elocationid" ] JS.str json >>= go
    where go x = case Tx.take 4 x of
                      ""     -> pure Tx.empty
                      "doi:" -> pure . Tx.drop 5 $ x
                      _      -> go . Tx.tail $ x

getPages :: JS.JSON -> Maybe T.PageRange
getPages json = do
    txt <- JS.lookupWith [ "pages" ] JS.str json
    case At.parseOnly parsePageNumbers txt of
         Right ps -> pure ps
         Left  _  -> pure T.Online

parsePage :: At.Parser T.PageNo
parsePage = do
    prefix <- many At.letter
    suffix <- P.unsigned'
    pure $ T.PageNo prefix suffix

parsePageNumbers :: At.Parser T.PageRange
parsePageNumbers = do
    start <- parsePage
    stop  <- (At.char '-' *> parsePage) <|> pure start
    pure $ T.InPrint start stop

parseDate :: At.Parser Day
parseDate = fromGregorian <$> P.unsigned <*> month <*> day
    where month  = At.skipSpace *> At.choice aMonth <* At.skipSpace
          day    = P.unsigned' <|> pure 0
          aMonth = [ At.string "Jan" *> At.skipSpace *> pure 1
                   , At.string "Feb" *> At.skipSpace *> pure 2
                   , At.string "Mar" *> At.skipSpace *> pure 3
                   , At.string "Apr" *> At.skipSpace *> pure 4
                   , At.string "May" *> At.skipSpace *> pure 5
                   , At.string "Jun" *> At.skipSpace *> pure 6
                   , At.string "Jul" *> At.skipSpace *> pure 7
                   , At.string "Aug" *> At.skipSpace *> pure 8
                   , At.string "Sep" *> At.skipSpace *> pure 9
                   , At.string "Oct" *> At.skipSpace *> pure 10
                   , At.string "Nov" *> At.skipSpace *> pure 11
                   , At.string "Dec" *> At.skipSpace *> pure 12
                   ]

-- =============================================================== --
-- Parsing Research Information Systems (RIS) citations
-- See: https://en.wikipedia.org/wiki/RIS_(file_format)

type ParsedRIS = [(Text,Text)]

parseCitationRIS :: Text -> Either T.ErrString T.Citation
parseCitationRIS txt = bimap go id $ parseRIS txt >>= readRIS
    where go err = unlines [ "Cannot parse Research Info Systems (RIS) format"
                           , err
                           , "Downloaded RIS:"
                           , Tx.unpack txt
                           ]

parseRIS :: Text -> Either T.ErrString ParsedRIS
-- ^If the parse is successful, then we need to remove any line
-- breaks and leading white space. If the parsed RIS starts with a
-- linebreak then it will get dropped. See the risPair parser.
parseRIS txt = bimap msg unbreak . At.parseOnly risParser $ txt
    where msg err                = "Parse Error: " <> err
          unbreak                = reverse . foldl' go []
          go []         ("br",_) = []
          go ((k,v):xs) ("br",x) = (k, v <> Tx.dropWhile isSpace x):xs
          go xs         x        = x:xs

risParser :: At.Parser ParsedRIS
risParser = do
    (start,_) <- risPair
    guard $ start == "TY"
    pairs <- some risPair
    stop <- risKey
    guard $ stop == "ER"
    At.skipSpace
    pure pairs

risPair :: At.Parser (Text,Text)
-- ^RIS is supposed to have every key-value pair on a single line
-- with no line breaks. However, some doi lookups return RIS with
-- fields that contain extra spaces and line breaks. As of this
-- comment (040121) the doi: 10.1002/anie.202014337 has this problem.
-- To handle this, any error in parsing a RIS key is assumed to be a
-- continuation of the value from the previous key-value pair. This
-- is indicated by the "br" key, which is not a standard RIS key.
-- The line break is removed in the process.
risPair = do
    key <- risKey <|> pure "br"
    guard $ key /= "ER"
    val <- At.takeTill At.isEndOfLine
    At.endOfLine
    pure (key, val)

risKey :: At.Parser Text
risKey = key <* hyphen
    where key    = fmap Tx.pack . replicateM 2 $ At.satisfy isAlphaNum
          hyphen = At.skipSpace *> At.char '-' *> P.horizontalSpaces

-- ------------------------------------------------------------------
-- Reading the RIS data into a citation

readRIS :: ParsedRIS -> Either T.ErrString T.Citation
readRIS ris = maybe err pure . risReader $ ris
    where err = Left $ "Unable to read parsed RIS:\n" <> concatMap show ris

risReader :: ParsedRIS -> Maybe T.Citation
risReader ris = T.Citation <$> risTitle   ris
                           <*> risAuthors ris
                           <*> risIssue   ris
                           <*> risPages   ris
                           <*> risDOI     ris
                           <*> risPMID    ris

risTitle :: ParsedRIS -> Maybe Text
risTitle = lookup "TI"

risAuthors :: ParsedRIS -> Maybe [Text]
risAuthors = pure . foldr go []
    where go ("AU",x) xs = x:xs
          go _        xs = xs

risDOI :: ParsedRIS -> Maybe Text
risDOI = lookup "DO"

risPMID :: ParsedRIS -> Maybe Text
risPMID = fmap fakePMID . risDOI

risPages :: ParsedRIS -> Maybe T.PageRange
-- "SP" is start page but may include the page range
-- "EP" is the end page.
risPages ris =
    let s = At.parseOnly parsePageNumbers . maybe "" id . lookup "SP" $ ris
        e = At.parseOnly parsePage        . maybe "" id . lookup "EP" $ ris
    in  case (s, e) of
             (Left _,                Left _ ) -> pure T.Online
             (Right x,               Left _ ) -> pure x
             (Left _,                Right y) -> pure $ T.InPrint y y
             (Right (T.InPrint x _), Right y) -> pure $ T.InPrint x y
             _                                -> pure T.Online

risIssue :: ParsedRIS -> Maybe T.Issue
risIssue ris = T.Issue <$> risDate ris
                       <*> ( risInt ris "VL" <|> pure 0 )
                       <*> ( risInt ris "IS" <|> pure 0 )
                       <*> risJournal ris

risJournal :: ParsedRIS -> Maybe T.Journal
risJournal = fmap go . lookup "T2"
    where go j = T.Journal j j j T.UnknownFreq True 20 False

risDate :: ParsedRIS -> Maybe Day
-- "PY" publication year, but may contain a date
-- "DA" date in the format YYYY/MM/DD
risDate ris = go "DA" <|> go "PY"
    where go x = lookup x ris >>= risParseDate

risInt :: ParsedRIS -> Text -> Maybe Int
risInt ris key = lookup key ris >>= C.readMaybeTxt

risParseDate :: Text -> Maybe Day
risParseDate txt = either (const Nothing) pure . At.parseOnly go $ txt
    where go = do y  <- P.unsigned
                  (At.char '/' *> pure ()) <|> At.endOfInput
                  md <- At.sepBy P.unsigned' $ At.char '/'
                  case md of
                       (m:[])   -> pure $ fromGregorian y m   1
                       (m:d:[]) -> pure $ fromGregorian y m   d
                       _        -> pure $ fromGregorian y 12 31
