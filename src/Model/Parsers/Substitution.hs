{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Substitution
    ( substitute
    , parseAtBraced
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict      as Map
import           Data.Text                   ( Text        )
import           Data.Bifunctor              ( bimap       )
import           Control.Applicative         ( (<|>), many )

data AtBraced =
      FreeAt
    | Braced Text
    | Unbraced Text deriving ( Eq, Show )

-- The parser does not fail, so we don't need to return an either.

substitute :: Text -> Map.Map Text Text -> Text -> Either String Text
substitute d xys = fmap (Tx.concat . map go) . parseAtBraced
    where go FreeAt       = "@"
          go (Unbraced y) = y
          go (Braced   x) = Map.findWithDefault d x xys

parseAtBraced :: Text -> Either String [AtBraced]
parseAtBraced = bimap err id . At.parseOnly ( many atBraced )
    where err = (++) "Cannot parse at-braced text: "

atBraced :: At.Parser AtBraced
atBraced = At.peekChar' >>= go
    where go '@' = braced <|> freeAt
          go  _  = unbraced

freeAt :: At.Parser AtBraced
freeAt = At.char '@' *> pure FreeAt

unbraced :: At.Parser AtBraced
unbraced = Unbraced <$> At.takeTill (=='@')

braced :: At.Parser AtBraced
braced = Braced <$> go
    where go = At.string "@{" *> At.takeTill (=='}') <* At.char '}'
