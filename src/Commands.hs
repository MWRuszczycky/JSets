{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( downloadToC
    , downloadToCTxt
    , writeTocsToMkd
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Network.Wreq              as Wreq
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified Model.Core.References     as R
import           Data.Text                          ( Text           )
import           Text.Read                          ( readMaybe      )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Except               ( ExceptT (..)
                                                    , liftIO
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- Acquiring journal sets

findJsets :: T.Config -> T.ErrMonad T.JournalSets
findJsets c = case (T.cJsetsFile c, T.cJsetsYear c) of
                   (Just fp, _      ) -> jsetsFromFile fp
                   (Nothing, Just y ) -> jsetsFromYear y
                   (Nothing, Nothing) -> throwError "Cannot find journal sets."

jsetsFromFile :: FilePath -> T.ErrMonad T.JournalSets
jsetsFromFile fp = ExceptT $ P.parseJsets <$> Tx.readFile fp

jsetsFromYear :: String -> T.ErrMonad T.JournalSets
jsetsFromYear = maybe err go . readMaybe
    where err  = throwError "Invalid year."
          go y = pure . J.yearly26Sets y $ R.issueRefs

getJset :: T.Config -> T.ErrMonad T.JournalSet
getJset c = do
    let err = "Cannot find requested journal set."
    jsets <- findJsets c
    key   <- getJsetNumber c
    jset  <- maybe (throwError err) pure . Map.lookup key $ jsets
    pure (key, jset)

getJsetNumber :: T.Config -> T.ErrMonad (Int, Int)
getJsetNumber c = let err    = throwError "Set number must be YEAR-NUMBER."
                      go u v = (,) <$> readMaybe u <*> readMaybe v
                  in  case break (== '-') <$> T.cJsetKey c of
                           Just (y,'-':n) -> maybe err pure . go y $ n
                           _              -> err

---------------------------------------------------------------------
-- Downloading table of contents

writeTocsToMkd :: T.Config -> T.ErrMonad Text
writeTocsToMkd c = do
    let defFP = "dev/tocs.mkd"
    (_,xs) <- getJset c
    pubmedData <- mapM downloadIssueToC xs
    liftIO . Tx.writeFile defFP . Tx.unlines . map F.tocToMkd $ pubmedData
    pure . Tx.pack $ "Tables of contents written to " <> defFP

downloadIssueToC ::  T.Issue -> T.ErrMonad T.TableOfContents
downloadIssueToC x = do
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    toc <- downloadToC x
    liftIO . Tx.putStrLn $ "OK"
    pure toc

downloadToC :: T.Issue -> T.ErrMonad T.TableOfContents
downloadToC x = downloadToCTxt x >>= liftEither . P.parseToC x

downloadToCTxt :: T.Issue -> T.ErrMonad Text
downloadToCTxt x = ExceptT $ J.readResponse <$> Wreq.getWith opts addr
    where addr = "https://www.ncbi.nlm.nih.gov/pubmed"
          opts = J.tocQuery x
