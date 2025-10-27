module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime) where

import Instances (Parser (..))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Parser (
    char, int, eof, satisfy, is, isNot, oneof, noneof, digit, space, spaces, spaces1,
    lower, upper, alpha, inlineSpace, inlineSpaces, inlineSpace1, string, tok, charTok, stringTok,
    commaTok, between, sepBy1
    )
import Control.Applicative (many, some, (<|>))
import Data.Char (toUpper)
import Data.List (intercalate)

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


{- Part A Helper parsers -}

nonTerminal :: Parser String
nonTerminal = between (is '<') (is '>') $ do
    first <- lower
    rest <- many (alpha <|> digit <|> is '_')
    pure (first : rest)

terminal :: Parser String
terminal = between (is '"') (is '"') (many (isNot '"'))

macro :: Parser String
macro = between (is '[') (is ']') (some alpha)

{- Part B helper functions -}

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize [] = []

symbolToType :: Symbol -> String
symbolToType (NonTerminal s) = capitalize s
symbolToType (Terminal _) = "String" 
symbolToType (Macro "int") = "Int"
symbolToType (Macro _) = "String"   -- [alpha] or [newline]

symbolParser :: Symbol -> String
symbolParser (NonTerminal n) = n
symbolParser (Terminal s)    = "(string \"" ++ s ++ "\")"
symbolParser (Macro "int")   = "int"
symbolParser (Macro _)       = "undefinedMacro"

altToConstructor :: String -> Int -> Alternative -> String
altToConstructor ruleName i (Alternative symbols) =
    constructor ++ " " ++ unwords (map symbolToType symbols)    
    where
        constructor = capitalize ruleName ++ show i

{-
constructs the rhs of the production rule 
unwords ["Term", "String", "Expression"] 
-> "Term String Expression" (space between each element)
-}

ruleToData :: Rule -> String
ruleToData (Rule name alts) = 
    case alts of
        [Alternative [s]] ->
            "newtype " ++ capName ++ " = " ++ capName ++ " " ++ symbolToType s ++ "\n    deriving Show" -- One alternative and one field 
        _ -> "data " ++ capName ++ " = " ++ intercalate ("\n" ++ (replicate l_lhs_data ' ') ++ "| ") (zipWith (altToConstructor name) [1..] alts) ++ "\n    deriving Show"
    where
        lhs_data        = "data " ++ capName ++ " "
        l_lhs_data      = length lhs_data
        capName         = capitalize name

altToParser :: String -> Int -> Alternative -> String
altToParser ruleName i (Alternative symbols) =
    constructor ++ " <$> " ++ intercalate " <*> " (map symbolParser symbols)
    where
        constructor = capitalize ruleName ++ show i

{-
intercalate -> insert "something" between each element and return it as a string
-}

ruleToParser :: Rule -> String
ruleToParser (Rule name [Alternative [s]]) =                -- If newtype, no numbered constructor
    name ++ " :: Parser " ++ capName ++ "\n"
    ++ name ++ " = " ++ capName ++ " <$> " ++ symbolParser s
  where
    capName = capitalize name

ruleToParser (Rule name alts) =
    name ++ " :: Parser " ++ capName ++ "\n"                -- data type, numbered constructor
    ++ name ++ " = "
    ++ intercalate ("\n" ++ (replicate (length name + 1) ' ') ++  "<|> ")
        (zipWith (altToParser capName) [1..] alts)
  where
    capName = capitalize name

generateHaskellCode :: ADT -> String
generateHaskellCode (Grammar rules) =
    typeDefs ++ "\n\n" ++ parserDefs ++ "\n"
    where
        typeDefs = intercalate "\n\n" (map ruleToData rules)
        parserDefs = intercalate "\n\n" (map ruleToParser rules)

validate :: ADT -> [String]
validate _ = ["If i change these function types, I will get a 0 for correctness"]

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
