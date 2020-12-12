{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module AppMonad
    ( -- Data structer construction & aquisition
      readJSets
    , getJSet
    , getJSets
      -- Working with configured reference issues
    , references
    , getIssue
    , resolveIssueWith
    , resolveIssue
      -- Running rank matchings
    , runMatch
    ) where

import qualified Data.Text                 as Tx
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Types          as T
import qualified Model.Journals            as J
import qualified Model.Matching            as Mt
import qualified Model.Parsers.JournalSets as P
import           Data.List                          ( find       )
import           Data.Text                          ( Text       )
import           Control.Monad.Reader               ( asks       )
import           Control.Monad.Except               ( lift
                                                    , throwError )

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

readJSets :: FilePath -> T.AppMonad (T.JSets T.Issue)
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

resolveIssueWith :: [T.Issue] -> T.Citation -> T.AppMonad T.Citation
-- ^Attempt to resolve the issue of a citation to a configured issue;
-- however, first check that the issues is not already provided in
-- the list of likely candidates. If it is not found in the candidate
-- list, then try to look it up from scratch (see resolveIssue).
resolveIssueWith refs x = maybe (resolveIssue x) res . find go $ refs
        where res i = pure $ x { T.pubIssue = i }
              go  i = (T.pubmed . T.journal) i == (T.pubmed . T.journal) x
                      && T.year  i == T.year  x
                      && T.issNo i == T.issNo x

resolveIssue :: T.Citation -> T.AppMonad T.Citation
-- ^Attempt to resolve the issue of a citation to a configured issue.
-- If successful, replace the citation's issue with the configured
-- issue. Otherwise, do nothing. This function is useful for setting
-- the issue of citations downloaded from PubMed without knowing from
-- which issue they come.
resolveIssue x = references >>= pure . maybe x id . go
    where name  = T.pubmed . T.journal $ x
          go rs = do r   <- find ( (== name) . T.pubmed . T.journal ) rs
                     let key = T.abbr . T.journal $ r
                     iss <- J.lookupIssue rs key ( T.volNo x, T.issNo x )
                     pure $ x { T.pubIssue = iss }

-- =============================================================== --
-- Rank matching

runMatch :: [(Text, [[Int]])] -> (Text, [Int]) -> T.AppMonad T.MatchResult
runMatch ranklists (title, indices) = pure $ Mt.match title indices ranklists
