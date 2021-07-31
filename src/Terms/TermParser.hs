{-# LANGUAGE InstanceSigs, LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-incomplete-patterns #-}

module Terms.TermParser where 

import Terms.Terms         ( Term(..) )
import Control.Applicative ( Alternative((<|>), empty, many) )
import Data.Foldable       ( asum )
import Data.Char           ( isSpace )
import Control.Monad       ()

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b 
    fmap g p = P $ \inp -> case parse p inp of 
        []         -> []
        [(v, out)] -> [(g v, out)]  

instance Applicative Parser where 
    pure :: a -> Parser a 
    pure v = P $ \inp -> [(v, inp)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) pg px = P $ \inp -> case parse pg inp of 
        []         -> []
        [(g, out)] -> parse (fmap g px) out 

instance Monad Parser where 
    (>>=) p f = P $ \inp -> case parse p inp of 
        []         -> []
        [(v, out)] -> parse (f v) out 

instance Alternative Parser where
    empty :: Parser a 
    empty = P $ const []

    (<|>) :: Parser a -> Parser a -> Parser a 
    (<|>) p q = P $ \inp -> case parse p inp of 
        []        -> parse q inp 
        [(v,out)] -> [(v,out)] 

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char 
item = P $ \case
    []     -> []
    (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat charPred = item >>= (\x -> if charPred x then pure x else empty)

stringSat :: (String -> Bool) -> Parser String
stringSat stringPred = many item >>= (\x -> if stringPred x then pure x else empty) 

space :: Parser ()
space = many (sat isSpace) >> pure ()

char :: Char -> Parser Char 
char c = sat (==c)

string :: [Char] -> Parser [Char]
string = traverse char 

symbol :: [Char] -> Parser [Char]
symbol s = space *> string s <* space

between :: Parser a -> Parser a -> Parser s -> Parser s
between open close value = open *> value <* close 

parens :: Parser [Char] -> Parser [Char]
parens = between (symbol "(") (symbol ")")

choice :: [Parser a] -> Parser a
choice = asum

sep :: Parser a -> Parser s -> Parser [a]
sep p s =  (:) <$> p <*> many (s >> p) <|> pure [] 

isRoot :: Foldable t => t Char -> Parser Char 
isRoot sig = sat (`elem` sig)

rootSym :: Foldable t => t Char -> Parser ([Term] -> Term)
rootSym sig = T <$> ((:[]) <$> isRoot sig)

var :: [Char] -> Parser Term 
var _ = V <$> ((,) <$> item <*> pure 1)

rootParser :: [Char] -> Parser Term 
rootParser sig = space *> (rootSym sig <* symbol "(") <*> sep (topLevel sig) (symbol ",") <* symbol ")"

constSym :: [Char] -> Parser Term 
constSym sig = rootSym sig <*> pure [] 

topLevel :: [Char] -> Parser Term 
topLevel sig = choice [rootParser sig, constSym sig, var sig]

getTerm :: [Char] -> String -> Term
getTerm sig s = fst $ head $ parse (topLevel sig) s