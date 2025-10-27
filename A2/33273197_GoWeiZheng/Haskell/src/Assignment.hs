module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime) where

import Instances (Parser (..))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Parser (
    char, int, eof, satisfy, is, isNot, oneof, noneof, digit, space, spaces, spaces1,
    lower, upper, alpha, inlineSpace, inlineSpaces, inlineSpace1, string, tok, charTok, stringTok,
    commaTok, between, sepBy1
    )
import Control.Applicative (many, some, (<|>))

data Grammar = Grammar [Rule]
    deriving (Show)

data Rule = Rule String [Alternative]
    deriving (Show)

data Alternative = Alternative [Symbol]
    deriving (Show)

data Symbol
    = NonTerminal String
    | Terminal String
    | Macro String
    deriving (Show)

type ADT = Grammar

bnfParser :: Parser ADT
bnfParser = do
    _ <- spaces                     -- ignore leading whitespaces
    rules <- sepBy1 rule spaces     -- parse rules separated by whitespaces
    _ <- spaces                     -- ignore trailing whitespaces
    eof
    pure (Grammar rules)

rule :: Parser Rule
rule = do
    lhs <- nonTerminal
    _ <- inlineSpace1
    _ <- string "::="
    _ <- inlineSpace1
    rhs <- sepBy1 alternative (inlineSpace1 *> is '|' <* inlineSpace1)
    pure (Rule lhs rhs)

alternative :: Parser Alternative
alternative = do
    symbols <- sepBy1 symbol inlineSpace1
    pure (Alternative symbols)

symbol :: Parser Symbol
symbol = 
    (NonTerminal <$> nonTerminal) <|>
    (Terminal <$> terminal) <|>
    (Macro <$> macro)


{- Helper parsers -}

nonTerminal :: Parser String
nonTerminal = between (is '<') (is '>') $ do
    first <- lower
    rest <- many (alpha <|> digit <|> is '_')
    pure (first : rest)

terminal :: Parser String
terminal = between (is '"') (is '"') (many (isNot '"'))

macro :: Parser String
macro = between (is '[') (is ']') (some alpha)

generateHaskellCode :: ADT -> String
generateHaskellCode _ = "-- But I cannot change the type of these three functions."

validate :: ADT -> [String]
validate _ = ["If i change these function types, I will get a 0 for correctness"]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
