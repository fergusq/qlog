module Parser where

import Control.Applicative
import Control.Monad.Identity

import ParserCombinators
import Logic

-- | Prelexer converts a list of characters to a list of 'LNToken Char'

prelex :: String -> [LNToken Char]
prelex s = prelex' s 1 1

prelex' []        _ _ = []
prelex' ('\n':cs) l k = LNToken '\n' l k : prelex' cs (l+1) 1
prelex' (c   :cs) l k = LNToken c    l k : prelex' cs l     (k+1)

-- Lexer

type TParser r = ParserT (LNToken Char) Identity r

tokenp :: TParser (LNToken String)
tokenp = lnToken (some (oneOfL "0123456789"))
         <|> identifier
         <|> acceptL ":-"
         <|> (((:"")<$>) <$> oneOfL "().,;=")

tokensp :: String -> TParser [LNToken String]
tokensp eof = many (many space *> tokenp) <* many space <* acceptL eof

-- Parser

type PParser r = ParserT (LNToken String) Identity r

programp :: [String] -> PParser [((String, Int), (Expr, Expr))]
programp eof = many factp <* acceptL eof

factp :: PParser ((String, Int), (Expr, Expr))
factp = do head@(Compound name params) <- callp
           acceptL [":-"]
           body <- orp
           acceptL ["."]
           return ((name, length params), (head, body))

operatorp :: [String] -> PParser Expr -> PParser Expr
operatorp ops subp = do e <- subp
                        es <- many (do op <- oneOfL ops
                                       e' <- subp
                                       return (content op, e'))
                        return $ foldl (\a (op, b) -> Compound op [a, b]) e es

orp :: PParser Expr
orp = operatorp [";"] andp

andp :: PParser Expr
andp = operatorp [","] unifyp

unifyp :: PParser Expr
unifyp = operatorp ["="] simplep

simplep :: PParser Expr
simplep = callp <|> (acceptL ["("] *> orp <* acceptL [")"])

callp :: PParser Expr
callp = do name <- identifierL
           acceptL ["("]
           args <- (
               ((:) <$> simplep <*> many (acceptL [","] *> simplep)) <* acceptL [")"]
             ) <|> (acceptL [")"] *> pure [])
           return $ Compound name args

-- Lexer and parser interface

lexCode :: String -> Either [ParsingError] [LNToken String]
lexCode code = runIdentity $ parse (tokensp [tEofChar]) (prelex $ code++[tEofChar])
  where
    tEofChar = '\0'

parseCode :: String -> Either [ParsingError] [((String, Int), (Expr, Expr))]
parseCode code = do tokens <- lexCode code
                    runIdentity $ parse (programp [pEofStr]) (tokens++[pEof])
                 where
                   pEofStr = "<EOF>"
                   pEof = LNToken pEofStr 0 0
