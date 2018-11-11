module Parser where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

import Data.Char
import qualified Data.Map as M

import ParserCombinators
import Logic

-- | Prelexer converts a list of characters to a list of 'LNToken Char'

prelex :: String -> [LNToken Char]
prelex s = prelex' s 1 1

prelex' []        _ _ = []
prelex' ('%' :cs) l k = prelex' cs' l k' where comment = takeWhile (/='\n') cs
                                               cs' = drop (length comment) cs
                                               k' = k + length comment
prelex' ('\n':cs) l k = LNToken '\n' l k : prelex' cs (l+1) 1
prelex' (c   :cs) l k = LNToken c    l k : prelex' cs l     (k+1)

-- Lexer

type TParser r = ParserT (LNToken Char) Identity r

tokenp :: TParser (LNToken String)
tokenp = lnToken (some (oneOfL "0123456789"))
         <|> identifier
         <|> acceptL ":-"
         <|> acceptL "->"
         <|> acceptL "\\+"
         <|> acceptL "\\="
         <|> acceptL "<="
         <|> acceptL ">="
         <|> quotep
         <|> (((:"")<$>) <$> oneOfL "()[]{}<>.,;=|+-*/%")

quotep :: TParser (LNToken String)
quotep = acceptL "\"" *> (
           (('"':)<$>) <$> lnToken (many $ (noneOfL "\\\"" <|> (oneOfL "\\" *> oneOfL "\"\\")))
         ) <* acceptL "\""

tokensp :: String -> TParser [LNToken String]
tokensp eof = many (many space *> tokenp) <* many space <* acceptL eof

-- Parser

type PParser r = ParserT (LNToken String) (State (M.Map String Int)) r

programp :: [String] -> PParser [((String, Int), (Expr, Expr))]
programp eof = some clausep <* acceptL eof

clausep :: PParser ((String, Int), (Expr, Expr))
clausep = do head@(Compound name params) <- callp <|> parp
             body <- (acceptL [":-"] *> orp) <|> pure (Compound "tosi" [])
             acceptL ["."]
             return ((name, length params), (head, body))

operatorp :: [String] -> PParser Expr -> PParser Expr
operatorp ops subp = do e <- subp
                        es <- many (do op <- oneOfL ops
                                       e' <- subp
                                       return (content op, e'))
                        return $ foldl (\a (op, b) -> Compound op [a, b]) e es

hornp :: PParser Expr
hornp = operatorp [":-"] orp

orp :: PParser Expr
orp = operatorp [";"] andp

andp :: PParser Expr
andp = operatorp [","] firstp

firstp = impliesp

impliesp :: PParser Expr
impliesp = operatorp ["->"] unifyp

unifyp :: PParser Expr
unifyp = operatorp ["on", "=", "\\=", "<", ">", "<=", ">="] sump

sump :: PParser Expr
sump = operatorp ["+", "-"] termp

termp :: PParser Expr
termp = operatorp ["*", "/", "%"] simplep

simplep :: PParser Expr
simplep = intp <|> varp <|> anonVarp <|> callp <|> listp <|> stringp <|> negp <|> parp

intp :: PParser Expr
intp = do s <- nextToken
          unless (all (`elem` "0123456789") (content s)) $
            parsingError s "int token"
          return . SymbolInt . read $ content s

varp :: PParser Expr
varp = do t <- peekToken
          s@(c:cs) <- nextL
          unless (isUpper c) $
            parsingError t "variable"
          vars <- lift get
          case M.lookup s vars of
            Just i -> return $ Variable i
            Nothing -> do let i = length vars
                          lift $ put (M.insert s i vars)
                          return $ Variable i

anonVarp :: PParser Expr
anonVarp = do t <- peekToken
              s@(c:cs) <- nextL
              unless (c == '_') $
                parsingError t "unused variable"
              vars <- lift get
              let i = length vars
              lift $ put (M.insert s i vars)
              return $ Variable i

callp :: PParser Expr
callp = do name <- identifierL
           args <- (
               acceptL ["("] *> ((:) <$> firstp <*> many (acceptL [","] *> firstp)) <* acceptL [")"]
             ) <|> (acceptL["("] *> acceptL [")"] *> pure []) <|> pure []
           return $ Compound name args

listp :: PParser Expr
listp = emptylistp <|> nonemptylistp

emptylistp :: PParser Expr
emptylistp = acceptL ["["] *> acceptL ["]"] *> pure (Compound "[]" [])

nonemptylistp :: PParser Expr
nonemptylistp = do acceptL ["["]
                   item <- firstp
                   items <- many (acceptL [","] *> firstp)
                   tail <- (acceptL ["|"] *> firstp) <|> (pure (Compound "[]" []))
                   acceptL ["]"]
                   return $ foldr (\a b -> Compound "." [a, b]) tail (item:items)

stringp :: PParser Expr
stringp = do s <- nextToken
             unless ((content s !! 0) == '\"') $
               parsingError s "string token"
             let str = drop 1 (content s)
             return $ foldr (\a b -> Compound "." [a, b]) (Compound "[]" []) $ map (SymbolInt . toInteger . ord) str

negp :: PParser Expr
negp = do acceptL ["\\+"]
          e <- simplep
          return $ Compound "\\+" [e]

parp :: PParser Expr
parp = acceptL ["("] *> hornp <* acceptL [")"]

-- Lexer and parser interface

lexCode :: String -> Either [ParsingError] [LNToken String]
lexCode code = runIdentity $ parse (tokensp [tEofChar]) (prelex $ code++[tEofChar])
  where
    tEofChar = '\0'

parseExpression :: String -> Either [ParsingError] Expr
parseExpression code = do tokens <- lexCode code
                          fst $ runState (parse (hornp <* acceptL [pEofStr]) (tokens++[pEof])) M.empty

parseClauses :: String -> Either [ParsingError] [((String, Int), (Expr, Expr))]
parseClauses code = do tokens <- lexCode code
                       fst $ runState (parse (programp [pEofStr]) (tokens++[pEof])) M.empty

pEofStr = "<EOF>"
pEof = LNToken pEofStr 0 0
