{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module AppMonad
    ( -- Data structer construction & aquisition
      readJSets
    , getJSet
    , getJSets
      -- Working with configured reference issues
    , references
    , getIssue
      -- Internet requests
    , downloadPMIDs
    , downloadCitations
    , downloadContent
    , downloadContents
      -- Running rank matchings
    , runMatch
    ) where

import qualified Data.Text                 as Tx
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Core           as C
import qualified Model.Core.Dates          as D
import qualified Model.Journals            as J
import qualified Model.Core.Hungarian      as Hn
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified View.View                 as V
import qualified View.Core                 as Vc
import           Network.Wreq.Session               ( newSession     )
import           Data.Text                          ( Text           )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , runExceptT
                                                    , liftEither
                                                    , lift
                                                    , throwError     )

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

readJSets :: FilePath -> T.AppMonad (T.JSets T.Selection)
-- ^Obtain a collection of journal sets from a file.
readJSets fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseJSets refs content of
         Left err    -> throwError err
         Right jsets -> pure jsets

getJSet :: T.HasIssue a => T.JSets a -> Maybe Int -> T.AppMonad (T.JSet a)
-- ^Find a single journal set from a collection of journal sets
-- consistent with any journal set key provided. If no key is
-- provided and there is only journal set in the collection then
-- return that single journal set.
getJSet (T.JSets []) _ = throwError "No journal sets!"
getJSet (T.JSets (j:[])) (Just k)
    | k == T.setNo j = pure j
    | otherwise      = throwError "Journal set does not match requested key!"
getJSet (T.JSets (j:[])) Nothing  = pure j
getJSet _     Nothing  = throwError "A valid journal set key is required!"
getJSet jsets (Just k) =
    let cannotFind = "Cannot find requested journal set!"
    in  case J.lookupJSet k jsets of
             Nothing   -> throwError cannotFind
             Just jset -> pure jset

getJSets :: T.HasIssue a => T.JSets a -> Maybe Int -> T.AppMonad (T.JSets a)
-- ^This is the same as getJSet, but returns a JSets collection. If
-- a key is provided, then the collection is restricted to the single
-- requested journal set. Otherwise, all the journal sets are returned.
getJSets (T.JSets []) _ = throwError "No journal sets!"
getJSets (T.JSets (j:[])) (Just k)
    | k == T.setNo j = pure . J.pack $ [j]
    | otherwise      = throwError "Journal set does not match requested key!"
getJSets (T.JSets (j:[])) Nothing  = pure . J.pack $ [j]
getJSets jsets Nothing  = pure jsets
getJSets jsets (Just k) =
    let cannotFind = "Cannot find requested journal set!"
    in  case J.lookupJSet k jsets of
             Nothing   -> throwError cannotFind
             Just jset -> pure . J.pack $ [jset]

-- =============================================================== --
-- Working with configured reference issues

references :: T.AppMonad T.References
references = asks T.cReferences

getIssue :: Text -> Int -> Int -> T.AppMonad T.Issue
getIssue abbr v n = references >>= maybe err pure . go
    where issMsg = Tx.unpack abbr <> " " <> show v <> ":" <> show n
          err    = throwError $ "Invalid issue: " <> issMsg
          go rs  = J.lookupIssue rs abbr (v,n)

-- =============================================================== --
-- PubMed Pipeline

downloadPMIDs :: T.HasIssue a => C.WebRequest -> a -> T.AppMonad [Text]
downloadPMIDs wreq iss = do
    let query = J.tocESearchQuery iss
    C.putTxtMIO $ "Downloading " <> V.showIssue iss <> " PMIDs..."
    result <- liftIO . runExceptT . wreq query $ J.eSearchUrl
    case result >>= P.parsePMIDs of
         Left err  -> C.putStrMIO err                    *> pure []
         Right []  -> C.putStrMIO "None found at PubMed" *> pure []
         Right ids -> C.putStrMIO "OK "                  *> pure ids

downloadCitations :: C.WebRequest -> [Text] -> T.AppMonad [T.Citation]
downloadCitations _    []    = pure []
downloadCitations wreq pmids = do
    C.putTxtMIO "Downloading Citations..."
    let query = J.tocESumQuery pmids
    result <- liftIO . runExceptT . wreq query $ J.eSummaryUrl
    case result >>= P.parseCitations of
         Left  err     -> C.putStrMIO err  *> pure []
         Right ([],cs) -> C.putStrMIO "OK" *> pure cs
         Right (ms,cs) -> let msg = Tx.unwords $ "Missing PMIDS:" : ms
                          in  C.putTxtMIO msg *> pure cs

downloadContent :: C.WebRequest -> T.Selection -> T.AppMonad T.Content
downloadContent wreq sel = do
    start <- liftIO D.readClock
    cites <- downloadPMIDs wreq sel >>= downloadCitations wreq
    delta <- liftIO . D.deltaClock $ start
    C.putTxtLnMIO $ " (" <> Vc.showPicoSec delta <> ")"
    -- PubMed allows at most 3 requests per second. We've alrady made
    -- two at this point, so we pause for another second.
    liftIO . D.wait $ 10^12
    pure $ T.Content sel cites

downloadContents :: [T.Selection] -> T.AppMonad [T.Content]
downloadContents xs = do
    wreq     <- C.webRequestIn <$> liftIO newSession
    contents <- mapM (downloadContent wreq) xs
    C.putTxtLnMIO "Done"
    pure contents

-- =============================================================== --
-- Rank matching

runMatch :: [(Text, [[Int]])] -> (Text, [Int]) -> T.AppMonad T.MatchResult
runMatch namedRankLists (title, indices) = do
    let (phantoms, cards) = J.matchCards indices namedRankLists
    (s, ms) <- liftEither . Hn.solveMax . concatMap T.cardScores $ cards
    pure $ T.MatchResult { T.matchTitle = title
                         , T.matchings  = map (assignMatch ms phantoms) cards
                         , T.matchScore = s
                         }

assignMatch :: [(Int,Int)] -> [Int] -> T.MatchCard -> (Text, [Text])
assignMatch ms missing card = ( name, foldr go [] ms )
    where name = T.cardName card
          cIDs = T.cardIDs card
          go (x,w) xs | elem w cIDs && elem x missing = "none"    : xs
                      | elem w cIDs                   = C.tshow x : xs
                      | otherwise                     = xs
