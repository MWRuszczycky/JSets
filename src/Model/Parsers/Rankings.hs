{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Rankings
    ( parseRankings
    ) where

import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import qualified Model.Parsers.Core   as P
import qualified Data.Attoparsec.Text as At
import           Data.List                    ( nub   )
import           Data.Bifunctor               ( bimap )
import           Data.Text                    ( Text  )
import           Control.Applicative          ( some  )

-- =============================================================== --
-- Local types

type IndexList = (Text, [Int])
type RankList  = (Text, [[Int]])

-- =============================================================== --
-- Interface

parseRankings :: Text -> Either T.ErrString ([IndexList], [RankList])
parseRankings = bimap err id . go
    where err  = (<>) "Invalid or unparsable rankings input: "
          go t = do (is, rs) <- At.parseOnly componentParser t
                    validateIndices is
                    validateRankLists rs
                    pure (is, rs)

validateIndices :: [IndexList] -> Either T.ErrString ()
validateIndices xs
    | repeatedInd   = Left "There are repeated indices."
    | repeatedNames = Left "There are repeated index names."
    | otherwise     = pure ()
    where (ns,ks)       = unzip xs
          repeatedInd   = any ( \ k -> nub k /= k ) ks
          repeatedNames = nub ns /= ns

validateRankLists :: [RankList] -> Either T.ErrString ()
validateRankLists = mapM_ go
    where bad rs     = concat rs /= (nub . concat) rs
          go (n, rs) | bad rs    = Left $ Tx.unpack n <> " has repeated ranks."
                     | otherwise = pure ()

-- =============================================================== --
-- Implementation

componentParser :: At.Parser ([IndexList], [RankList])
componentParser = do
    P.comments
    xs <- At.sepBy1 indexList P.comments
    P.comments
    rs <- At.sepBy1 rankList  P.comments
    P.comments
    At.endOfInput
    pure (xs, rs)

indexList :: At.Parser IndexList
indexList = do
    At.string "indices"
    P.colon
    name    <- validName
    At.skipSpace
    indices <- At.sepBy P.unsigned'(some At.space)
    pure (name, indices)

rankList :: At.Parser RankList
rankList = do
    name <- validName
    P.colon
    rs   <- ranks
    pure (name, rs)

ranks :: At.Parser [[Int]]
ranks = At.sepBy equalRanks P.gtSign

equalRanks :: At.Parser [Int]
equalRanks = At.sepBy1 P.unsigned' P.equalSign

validName :: At.Parser Text
validName = fmap Tx.pack . some . At.choice $ ok
    where ok = [ At.letter, At.digit, At.char '-', At.char '_' ]
