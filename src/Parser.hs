module Parser where

import Control.Applicative
import Control.Monad
import AnimationLib
import Prelude hiding (EQ, LT, GT)
import Data.Char (isAlphaNum)

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

strict :: Parser a -> String -> Maybe a
strict (P p) s = case p s of
    Just (r, "") -> Just r
    _ -> Nothing

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

alphanum :: Parser Char
alphanum = sat isAlphaNum

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

-- look for parens around a parser
parens :: Parser a -> Parser a
parens p = do
    token $ char '('
    r <- p
    token $ char ')'
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

-- parser for floating points
float :: Parser Float
float = do
    x <- int
    char '.'
    y <- nat
    return ((fromIntegral x) + (fromIntegral y) / 10)
    <|> do
    x <- int
    return $ fromIntegral x


-- parse a sequence of a given parser with the given beginning, end, and separator chars
sequenceOf :: Char -> Char -> Char -> Parser a -> Parser [a]
sequenceOf b e s p = do
    token (char b)
    r <- token p
    rs <- many ((token $ char s) >> p)
    token $ char e
    return (r:rs)

-- parse a list of a given parser, surrounded by brackets and separated by commas
listOf :: Parser a -> Parser [a]
listOf = sequenceOf '[' ']' ','

-- parsers for our lang


-- literals, object fields and operators--

floatLit :: Parser (Expr Float)
floatLit = do
    x <- token $ float
    return (Lit x)

boolLit :: Parser (Expr Bool)
boolLit = do
    b <- token $ string "True"
    return (Lit True)
    <|>
          do
    b <- token $ string "False"
    return (Lit False)

-- parser for functions of type Int -> Int -> Int
arithmetic :: Parser (Function (Float -> Float -> Float))
arithmetic = do
    o <- token $ inList ["Add", "Mul", "Sub"]
    case o of
        "Add" -> return Add
        "Mul" -> return Mul
        "Sub" -> return Sub

-- parser for binary boolean functions
logical :: Parser (Function (Bool -> Bool -> Bool))
logical = do
    o <- token $ inList ["And", "Or"]
    case o of
        "And" -> return And
        "Or"  -> return Or

comparison :: Ord a => Parser (Function (a -> a -> Bool))
comparison = do
    o <- token $ inList ["GT", "LT", "EQ"]
    case o of
        "LT" -> return LT
        "GT" -> return GT
        "EQ" -> return EQ

-- parse the fields fo objects
stringField :: Parser (ObjectField String)
stringField = do
    f <- token $ inList ["Name", "Disp"]
    case f of
        "Name" -> return Name
        "Disp" -> return Disp

floatField :: Parser (ObjectField Float)
floatField = do
    f <- token $ inList ["PosX", "PosY", "PosZ", "Size", "Dir"]
    case f of
        "PosX" -> return PosX
        "PosY" -> return PosY
        "PosZ" -> return PosZ
        "Size" -> return Size
        "Dir"  -> return Dir


-- more complex expressions

-- parse numerical exprs up to Binary operators
nexpr :: Parser (Expr Float)
nexpr = do
    o <- token arithmetic
    l <- token nexpr
    r <- token nexpr
    return (Bin o l r)
    <|> token floatLit

    <|> token (ifExpr nexpr)

-- parse simple bool exprs and comparisons
bexpr :: Parser (Expr Bool)
bexpr = do
    o <- token logical
    l <- token bexpr
    r <- token bexpr
    return (Bin o l r)

    <|> token boolLit

    <|> token (compExpr bexpr)  -- instances of comparisons
    <|> token (compExpr nexpr)

-- parse a comparison using the given parser for both sides of the expression
compExpr :: Ord a => Parser (Expr a) -> Parser (Expr Bool)
compExpr p = do
    o <- token comparison
    l <- token p
    r <- token p
    return (Bin o l r)

-- parse a conditional with Then and Else branches coming from a given parser
ifExpr :: Parser (Expr a) -> Parser (Expr a)
ifExpr p = do
    token $ string "If"
    c <- parens bexpr
    token $ string "Then"
    t <- parens p
    token $ string "Else"
    e <- parens p
    return (If c t e)

-- how to implement Get???


transformation :: Parser Transformation
transformation = do
    o <- token $ inList ["Pivot", "Move", "Grow", "Step", "Wait", "Combine"]
    case o of
        "Pivot" -> do
            e <- nexpr
            return $ Pivot e
        "Move" -> do
            x <- nexpr
            y <- nexpr
            return $ Move x y
        "Grow" -> do
            e <- nexpr
            return $ Grow e
        "Step" -> do
            e <- nexpr
            return $ Step e
        "Wait" -> return Wait
        "Combine" -> do
            ts <- listOf transformation
            return $ Combine ts

-- imposing some syntax for timed transformations, "time -> transformation"
timedTransformation :: Parser TimedTransformation
timedTransformation = do
    s <- token float
    string "->"
    t <- transformation
    return (s, t)

-- AnimationSequences are bracketed by {} and separated by ;
animationSeq :: Parser AnimationSeq
animationSeq = sequenceOf '{' '}' ';' timedTransformation


-- data Object =
--   Object
--     { name :: String,
--       disp :: String,
--       posx :: Float,
--       posy :: Float,
--       posz :: Float,
--       size :: Float,
--       dir :: Float
--     } deriving (Show)
--

-- tentative (very simple) object declaration syntax
object :: Parser Object
object = do
    token $ string "Obj"
    token $ char '('
    n <- token $ some alphanum
    token $ char ','
    d <- token $ some alphanum
    token $ char ','
    x <- token $ float
    token $ char ','
    y <- token $ float
    token $ char ','
    z <- token $ float
    token $ char ','
    size <- token $ float
    token $ char ','
    dir <- token $ float
    token $ char ')'
    return (Object n d x y z size dir)

stmt :: Parser (Object, AnimationSeq)
stmt = do
    o <- object
    token $ string "->"
    a <- animationSeq
    return (o, a)

parseInput :: String -> Maybe (Object, AnimationSeq)
parseInput s = case parse stmt s of
                    Just (t, "") -> Just t
                    _ -> Nothing
