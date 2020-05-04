{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Templates
    ( substitute
    , parseAtBraced
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict      as Map
import           Data.Text                   ( Text        )
import           Control.Applicative         ( (<|>), many )

data AtBraced =
      FreeAt
    | Braced Text
    | Unbraced Text deriving ( Eq, Show )

substitute :: Text -> Map.Map Text Text -> Text -> Text
substitute d xys = Tx.concat . map go . parseAtBraced
    where go FreeAt       = "@"
          go (Unbraced y) = y
          go (Braced   x) = Map.findWithDefault d x xys

parseAtBraced :: Text -> [AtBraced]
-- ^This parser never fails, so we have a dummy value in the either
-- expression that just returns and empty list.
parseAtBraced = either (const []) id . At.parseOnly ( many atBraced )

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
