module SExpr where

import AParser
import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- | Parse zero or more occurences
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = many_p
  where
    -- Attempt to parse a single value.
    -- If it fails, produce an empty list.
    many_p = some_p <|> pure []

    -- Construct a list over the result of the Parser
    some_p = fmap (:) p <*> many_p

-- | Parse one or more occurences
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = some_p
  where
    -- Attempt to parse a single value.
    -- If it fails, produce an empty list.
    many_p = some_p <|> pure []

    -- Construct a list over the result of the Parser
    some_p = fmap (:) p <*> many_p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = fmap (:) (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer
          | I Ident
          deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
           deriving Show

atom :: Parser Atom
atom = fmap N posInt <|> fmap I ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *>
             (fmap A atom <|>
             char '(' *> fmap Comb (oneOrMore parseSExpr) <* char ')')
             <* spaces 
