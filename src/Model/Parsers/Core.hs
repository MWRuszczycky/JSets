{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Core
    ( unsigned
    , unsigned'
    , dateN
    , dateP
    , quoted
    , escaped
    , leftBrace
    , rightBrace
    , leftBracket
    , rightBracket
    , horizontalSpaces
    , spacesToEoL
    , comma
    , colon
    , colon'
    , pipe
    , pipe'
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import           Data.Time                   ( Day, fromGregorian )
import           Data.Text                   ( Text               )
import           Control.Applicative         ( (<|>), many, some  )

-- =============================================================== --
-- General parsers

---------------------------------------------------------------------
-- Specific data types

unsigned :: At.Parser Integer
unsigned = some At.digit >>= pure . read

unsigned' :: At.Parser Int
unsigned' = fromIntegral <$> unsigned

dateN :: At.Parser Day
dateN = do
    y <- unsigned  <* At.char '-'
    m <- unsigned' <* At.char '-'
    d <- unsigned'
    pure $ fromGregorian y m d

dateP :: At.Parser Day
dateP = At.char '(' *> dateN <* At.char ')'

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
-- Spaced separators

horizontalSpaces :: At.Parser ()
horizontalSpaces = At.skipWhile At.isHorizontalSpace

spacesToEoL :: At.Parser ()
spacesToEoL = horizontalSpaces <* At.endOfLine

colon :: At.Parser ()
colon = At.skipSpace *> At.char ':' *> At.skipSpace

colon' :: At.Parser ()
colon' = horizontalSpaces *> At.char ':' *> horizontalSpaces

comma :: At.Parser ()
comma = At.skipSpace *> At.char ',' *> At.skipSpace

pipe :: At.Parser ()
pipe = At.skipSpace *> At.char '|' *> At.skipSpace

pipe' :: At.Parser ()
pipe' = horizontalSpaces *> At.char '|' *> horizontalSpaces
