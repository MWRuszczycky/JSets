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

findJsets :: T.Setup -> T.ErrMonad T.JournalSets
findJsets su = case (T.suJsetsFile su, T.suJsetsYear su) of
                    (Just fp, _      ) -> jsetsFromFile fp
                    (Nothing, Just y ) -> jsetsFromYear y
                    (Nothing, Nothing) -> throwError "Cannot find journal sets."

jsetsFromFile :: FilePath -> T.ErrMonad T.JournalSets
jsetsFromFile fp = ExceptT $ P.parseJsets <$> Tx.readFile fp

jsetsFromYear :: String -> T.ErrMonad T.JournalSets
jsetsFromYear = maybe err go . readMaybe
    where err  = throwError "Invalid year."
          go y = pure . J.yearly26Sets y $ R.issueRefs

getJset :: T.Setup -> T.ErrMonad T.JournalSet
getJset su = do
    let err = "Cannot find requested journal set."
    jsets <- findJsets su
    key   <- getJsetNumber su
    jset  <- maybe (throwError err) pure . Map.lookup key $ jsets
    pure (key, jset)

getJsetNumber :: T.Setup -> T.ErrMonad (Int, Int)
getJsetNumber su = let err    = throwError "Set number must be YEAR-NUMBER."
                       go u v = (,) <$> readMaybe u <*> readMaybe v
                   in  case break (== '-') <$> T.suJsetKey su of
                            Just (y,'-':n) -> maybe err pure . go y $ n
                            _              -> err

---------------------------------------------------------------------
-- Downloading table of contents

writeTocsToMkd :: T.Setup -> T.ErrMonad Text
writeTocsToMkd su = do
    let defFP = "dev/tocs.mkd"
    (_,xs) <- getJset su
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
