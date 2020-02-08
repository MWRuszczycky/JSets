{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Model.Core.References     as R
import qualified Model.Journals            as J
import qualified Viewer                    as V

import qualified Network.Wreq                  as Wreq
import qualified Model.Core.Core               as C
import qualified Data.Text                     as Tx
import qualified Model.Core.Dates              as D
import           Lens.Micro                             ( (^.)          )
import           Data.Text.Encoding                     ( decodeUtf8    )
import           Data.ByteString.Lazy                   ( toStrict      )
import           Data.List                              ( intercalate   )
import           Data.Char                              ( isSpace       )

tocAddex :: String
-- It doesn't like the brackets.
tocAddex = "https://www.ncbi.nlm.nih.gov/pubmed?term=mark+ruszczycky[author]"

controller :: IO ()
controller = do
    let Just tocAdd = tocAddress <$> J.lookupIssue "JACS" (140,49)
        exAdd       = "https://example.com"
        exAdd1      = "https://www.ncbi.nlm.nih.gov/pubmed"
    putStrLn tocAddex
    toc <- connect tocAddex
    Tx.putStrLn toc

tocAddress :: T.Issue -> String
-- https://www.ncbi.nlm.nih.gov/books/NBK3862/
tocAddress x = b ++ buildQuery [j, y, n]
    where b = "https://www.ncbi.nlm.nih.gov/pubmed?term="
          y = ( ++ "[year]"    ) . show . D.getYear . T.date $ x
          n = ( ++ "[issue]"   ) . show . T.issNo $ x
          j = ( ++ "[journal]" ) . Tx.unpack . T.pubmed . T.journal $ x

buildQuery :: [String] -> String
buildQuery = intercalate "+AND+" . (map . map) go
    where go x | isSpace x = '+'
               | otherwise = x

connect :: String -> IO Tx.Text
connect url = do
    r <- Wreq.get url
    let code = C.txt $ r ^. Wreq.responseStatus . Wreq.statusCode
        body = decodeUtf8 . toStrict $ r ^. Wreq.responseBody
    pure body
