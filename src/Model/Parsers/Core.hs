{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Core
    ( quoted
    , escaped
    , leftBrace
    , rightBrace
    , leftBracket
    , rightBracket
    , comma
    , colon
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text        )
import           Control.Applicative         ( (<|>), many )

-- =============================================================== --
-- General parsers

---------------------------------------------------------------------
-- Strings

quoted :: At.Parser Text
quoted = do
    At.char '\"'
    s <- many $ escaped <|> At.satisfy ( \ x -> x /= '\\' && x /= '\"' )
    At.char '\"'
    pure . Tx.pack $ s

escaped :: At.Parser Char
escaped = At.char '\\' *> At.choice [ At.char '\"' *> pure '\"'
                                    , At.char '/'  *> pure '/'
                                    , At.char 'b'  *> pure '\b'
                                    , At.char 'f'  *> pure '\f'
                                    , At.char 'n'  *> pure '\n'
                                    , At.char 'r'  *> pure '\r'
                                    , At.char 't'  *> pure '\t'
                                    , At.char 'u'  *> pure 'u'
                                    , At.char '\\' *> pure '\\'
                                    ]

---------------------------------------------------------------------
-- Braces and brackets

leftBrace :: At.Parser ()
leftBrace = At.skipSpace *> At.char '{' *> At.skipSpace

rightBrace :: At.Parser ()
rightBrace = At.skipSpace *> At.char '}' *> At.skipSpace

leftBracket :: At.Parser ()
leftBracket = At.skipSpace *> At.char '[' *> At.skipSpace

rightBracket :: At.Parser ()
rightBracket = At.skipSpace *> At.char ']' *> At.skipSpace

---------------------------------------------------------------------
-- Separators

colon :: At.Parser ()
colon = At.skipSpace *> At.char ':' *> At.skipSpace

comma :: At.Parser ()
comma = At.skipSpace *> At.char ',' *> At.skipSpace
