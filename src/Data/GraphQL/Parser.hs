{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
{-|
Module      : Data.GraphQL.Parser
Description : The parser for GraphQL Parsers.
Copyright   : (c) Nicholas Dujay, 2016
License     : MIT
Maintainer  : nickdujay@gmail.com
Stability   : experimental
Portability : non-portable
Language    : Haskell2010
-}
module Data.GraphQL.Parser
  ( parseDocument
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Text

import Data.GraphQL.Document

import qualified Text.Megaparsec.Lexer as L

-- lexer functions
void :: Parser a -> Parser ()
void p = p >> return ()

whitespace :: Parser ()
whitespace = space <|> (void tab)

terminators :: Parser ()
terminators = void eol

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (whitespace <|> terminators) lineComment (return ())

symbol :: String -> Parser String
symbol = L.symbol scn

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
comma = symbol ","

-- tokens
punctuator :: Parser String
punctuator = L.lexeme scn $ (pure <$> oneOf "!$():=@[]{}") <|> string "..."

underscore :: Parser Char
underscore = char '_'

nameStart :: Parser Char
nameStart = asciiChar <|> underscore

nameLetter :: Parser Char
nameLetter = asciiChar <|> digitChar <|> underscore

name :: Parser String
name = L.lexeme scn $ (:) <$> nameStart <*> many nameLetter

signed :: Num a => Parser a -> Parser a
signed p = ($) <$> option id (char '-' *> return negate) <*> p

intValue :: Parser Integer
intValue = L.lexeme scn $ signed L.integer

floatValue :: Parser Double
floatValue = L.lexeme scn $ signed L.float

escapedUnicode :: Parser String
escapedUnicode = (++) <$> string "\\u" <*> count 4 (oneOf "ABCDEFabcdef" <|> digitChar)

escapedChararacter :: Parser String
escapedChararacter = (:) <$> char '\\' <*> (pure <$> oneOf "\"\\/bfnrt")

stringChar :: Parser String
stringChar = (pure <$> noneOf "\"\n\r") <|> escapedUnicode <|> escapedChararacter

stringValue :: Parser String
stringValue = L.lexeme scn $ concat <$> between (char '"') (char '"') (many stringChar)

-- documents
document :: Parser Document
document = Document <$> many definition

definition :: Parser Definition
definition = operation <|> selectionSet <|> fragment

-- operations
operation :: Parser Definition
operation = do
  t <- operationType
  n <- option Nothing (Just <$> name)
  return $ Operation t n [] [] (SelectionSet [])

operationType :: Parser OperationType
operationType = (symbol "query" >> return Query) <|> (symbol "mutation" >> return Mutation)

-- selections
selectionSet :: Parser Definition
selectionSet = undefined

selection :: Parser Selection
selection = field <|> fragmentSpread <|> inlineFragment

field :: Parser Selection
field = undefined

fragmentSpread :: Parser Selection
fragmentSpread = undefined

inlineFragment :: Parser Selection
inlineFragment = undefined

-- fragments
fragment :: Parser Definition
fragment = undefined

parseDocument :: IO ()
parseDocument = undefined
