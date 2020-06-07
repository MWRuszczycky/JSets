{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Rankings
    ( parseRankings
    ) where

import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import qualified Model.Parsers.Core   as P
import qualified Data.Attoparsec.Text as At
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
parseRankings = bimap err id . At.parseOnly componentParser
    where err = (<>) "Cannot parse rankings: "

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
ranks = At.sepBy1 equalRanks P.gtSign

equalRanks :: At.Parser [Int]
equalRanks = At.sepBy1 P.unsigned' P.equalSign

validName :: At.Parser Text
validName = fmap Tx.pack . some . At.choice $ ok
    where ok = [ At.letter, At.digit, At.char '-', At.char '_' ]
