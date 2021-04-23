{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LookML.Parser
-- Description : Implementation of LookML Parser in Parsec
-- Copyright   : (c) Farid Zakaria, 2021
-- License     : MIT
-- Maintainer  : farid.m.zakaria@gmail.com
module Parser
  ( Parser.parse,
    Parser.pretty,
    Parser.parseTest,
  )
where

import Control.Applicative hiding (many, some)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void
import Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.PrettyPrint as Pretty

-- Our custom parser
type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

data LookMLValue
  = SQL String
  | Identifier String
  | Text String
  | Number Integer
  | Object (Map.Map String LookMLValue)
  | NamedObject String (Map.Map String LookMLValue)
  | Sequence [LookMLValue]
  | Array [LookMLValue]
  | Property String LookMLValue
  | YesNo Bool
  | TrueFalse Bool
  | DayOfWeek DayOfWeek
  | Interval Interval
  | DateType DateType
  | Disjunction LookMLValue LookMLValue
  deriving (Show)

data DateType
  = Epoch
  | Timestamp
  | DateTime
  | Date
  | YYMMDD
  deriving (Show)

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

data Interval
  = Day
  | Hour
  | Minute
  | Month
  | Quarter
  | Second
  | Week
  | Year
  deriving (Show)

-- https://gist.github.com/LightAndLight/36926a4e3a7133910d9aa199da50c4fd
-- delimiter :: Parser ()
-- delimiter = choice [void $ char ';', void spaceChar, eof]

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- A space consumer WITHOUT new lines
sc :: Parser ()
sc = L.space hspace1 lineComment empty

-- A space consumer WITH new lines
scn :: Parser ()
scn = L.space space1 lineComment empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

colon :: Parser Text
colon = symbol ":"

newlines :: Parser ()
newlines = skipMany newline

braces :: Parser a -> Parser a
braces = between (symbol "{" <* space) (space *> symbol "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

keyword :: Text -> Parser Text
keyword = lexeme <$> string

interval =
  Interval
    <$> ( (keyword "day" $> Day)
            <|> (keyword "hour" $> Hour)
            <|> (keyword "minute" $> Minute)
            <|> (keyword "month" $> Month)
            <|> (keyword "quarter" $> Quarter)
            <|> (keyword "second" $> Second)
            <|> (keyword "week" $> Week)
            <|> (keyword "year" $> Year)
        )

dayOfWeek =
  DayOfWeek
    <$> ( (keyword "monday" $> Monday)
            <|> (keyword "tuesday" $> Tuesday)
            <|> (keyword "wednesday" $> Wednesday)
            <|> (keyword "thursday" $> Thursday)
            <|> (keyword "friday" $> Friday)
            <|> (keyword "saturday" $> Saturday)
            <|> (keyword "sunday" $> Sunday)
        )

dateType =
  DateType
    <$> ( (keyword "epoch" $> Epoch)
            <|> (keyword "timestamp" $> Timestamp)
            <|> (keyword "datetime" $> DateTime)
            <|> (keyword "date" $> Date)
            <|> (keyword "yymmdd" $> YYMMDD)
        )

yesno :: Parser LookMLValue
yesno =
  (keyword "yes" $> YesNo True)
    <|> (keyword "no" $> YesNo False)

bool :: Parser LookMLValue
bool =
  (keyword "true" $> TrueFalse True)
    <|> (keyword "false" $> TrueFalse False)

number :: Parser LookMLValue
number = Number <$> lexeme L.decimal

property :: Parser LookMLValue
property = do
  _ <- scn -- consume any whitespace at the start
  i <- identifier
  _ <- colon
  Property i <$> value

object :: Parser LookMLValue
object = do
  p <- braces properties
  let list = fmap (\(Property n v) -> (n, v)) p
  return (Object $ Map.fromList list)

namedObject = do
  identifier <- identifier
  object <- object
  return
    ( case object of
        (Object map) -> NamedObject identifier map
    )

value :: Parser LookMLValue
value =
  yesno
    <|> number
    <|> bool
    <|> interval
    <|> dayOfWeek
    <|> dateType
    <|> object
    <|> namedObject
    <|> (Identifier <$> identifier)

properties :: Parser [LookMLValue]
properties = try property `sepEndBy` newlines

parser :: Parser LookMLValue
parser = scn *> (Sequence <$> properties) <* scn <* eof

parse :: Text -> Either ParserError LookMLValue
parse = Parsec.parse parser "<stdin>"

parseTest = Parsec.parseTest parser

pretty :: LookMLValue -> Pretty.Doc
pretty value' = case value' of
  (SQL s) -> Pretty.text s Pretty.<+> Pretty.semi Pretty.<> Pretty.semi
  (Identifier s) -> Pretty.text s
  (Property name v) -> Pretty.text name Pretty.<+> Pretty.colon Pretty.<+> pretty v
  (YesNo True) -> Pretty.text "yes"
  (YesNo False) -> Pretty.text "no"
  (Number n) -> Pretty.text (show n)
  (Sequence s) -> foldl (Pretty.$+$) Pretty.empty (pretty <$> s)
  (Object m) -> Pretty.braces (Map.foldlWithKey (\result k v -> result Pretty.$+$ pretty (Property k v)) Pretty.empty m)
  (NamedObject name m) -> Pretty.text name Pretty.<+> Pretty.braces (Map.foldlWithKey (\result k v -> pretty (Property k v) Pretty.$+$ result) Pretty.empty m)
  _ -> Pretty.empty