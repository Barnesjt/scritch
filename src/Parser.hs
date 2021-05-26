module Parser where

import Control.Applicative
import Control.Monad
import AnimationLib
import Prelude hiding (EQ, LT, GT)

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
    return a = P (\s -> Just (a, s))
    p >>= q  = P (\s -> case parse p s of
        Nothing -> Nothing
        Just (r, s') -> parse (q r) s')

instance Alternative Parser where
    empty = P (\_ -> Nothing)
    (P p) <|> (P q) = P (\s -> case p s of
        Nothing -> q s
        s' -> s')

-- applies a parser
parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

-- get the first character
item :: Parser Char
item = P (\s -> case s of
    [] -> Nothing
    (c:cs) -> Just (c, cs))

-- see if the first character satisfies a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

-- match a character
char :: Char -> Parser Char
char = sat . (==)

-- match from a list of characters
oneOf :: [Char] -> Parser Char
oneOf = sat . (flip elem)

-- special cases of oneOf
lower, upper, digit, space :: Parser Char
lower = oneOf ['a'..'z']
upper = oneOf ['A'..'Z']
digit = oneOf ['0'..'9']
space = oneOf " \t\n\r"

-- match a string
string :: String -> Parser String
string [] = return []
string (c:cs) = do
    x  <- char c
    xs <- string cs
    return (x:xs)

-- match a string from a list of strings
inList :: [String] -> Parser String
inList = foldr ((<|>) . string) empty

-- ignore whitespace
token :: Parser a -> Parser a
token p = do
    many space  -- many comes from Alternative
    r <- p
    many space
    return r

-- parse a digit, but as an integer this time
digitInt :: Parser Int
digitInt = do
    x <- digit
    return (fromEnum x - fromEnum '0')

-- parse consecutive digits into a (nonnegative) integer
nat :: Parser Int
nat = do
    xs <- some digitInt
    return (sum (zipWith (*) (reverse xs) (map (10^) [0, 1 ..])))

-- nat but can also parse negative ints
int :: Parser Int
int = nat <|> do
    char '-'
    x <- nat
    return (-x)


-- parsers for our lang

intLit :: Parser (Expr Int)
intLit = do
    x <- int
    return (Lit x)

boolLit :: Parser (Expr Bool)
boolLit = do
    b <- string "True"
    return (Lit True)
    <|>
          do
    b <- string "False"
    return (Lit False)

-- parser for functions of type Int -> Int -> Int
arithmetic :: Parser (Function (Int -> Int -> Int))
arithmetic = do
    o <- inList ["Add", "Mul", "Sub"]
    case o of
        "Add" -> return Add
        "Mul" -> return Mul
        "Sub" -> return Sub

-- parser for binary boolean functions
logical :: Parser (Function (Bool -> Bool -> Bool))
logical = do
    o <- inList ["And", "Or"]
    case o of
        "And" -> return And
        "Or"  -> return Or

comparison :: Ord a => Parser (Function (a -> a -> Bool))
comparison = do
    o <- inList ["GT", "LT", "EQ"]
    case o of
        "LT" -> return LT
        "GT" -> return GT
        "EQ" -> return EQ

iexpr :: Parser (Expr Int)
iexpr = do
    o <- token arithmetic
    l <- iexpr
    r <- token iexpr
    return (Bin o l r)
    <|> token intLit
