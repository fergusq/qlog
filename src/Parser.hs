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
         <|> acceptL "-->"
         <|> acceptL "\\+"
         <|> acceptL "\\="
         <|> acceptL "=.."
         <|> acceptL "<="
         <|> acceptL ">="
         <|> acceptL "!;"
         <|> acceptL "!."
         <|> acceptL "//"
         <|> quotep
         <|> ((:"")<$>) <$> oneOfL "()[]{}<>.,;=|+-*/%"

quotep :: TParser (LNToken String)
quotep = acceptL "\"" *>
           ((('"':)<$>) <$> lnToken (many (noneOfL "\\\"" <|> (oneOfL "\\" *> oneOfL "\"\\"))))
         <* acceptL "\""

tokensp :: String -> TParser [LNToken String]
tokensp eof = many (many space *> tokenp) <* many space <* acceptL eof

-- Parser

type PParser r = ParserT (LNToken String) (State Int) r

data Statement = ClauseDeclaration ((String, Int), Clause)
               | Directive Expr

programp :: [String] -> PParser [Statement]
programp eof = some statementp <* acceptL eof

statementp :: PParser Statement
statementp = ClauseDeclaration <$> (clausep <|> dcgp)
             <|> Directive <$> directivep

directivep :: PParser Expr
directivep = acceptL [":-"] *> orp <* acceptL ["."]

clausep :: PParser ((String, Int), Clause)
clausep = do head@(Compound name params) <- callp <|> parp
             body <- (acceptL [":-"] *> orp) <|> pure (Compound "tosi" [])
             mode <- (acceptL ["."] *> pure NoCut) <|> (acceptL ["!."] *> pure Cut)
             return ((name, length params), (head, body, mode))

dcgp :: PParser ((String, Int), Clause)
dcgp = do head@(Compound name params) <- callp <|> parp
          semicontext <- Just <$> (acceptL [","] *> dcgListp) <|> pure Nothing
          acceptL ["-->"]
          start <- newVar
          end <- newVar
          let head' = Compound name (params ++ [start, end])
          body <- dcgBodyp <|> dcgEmptyBodyp
          body' <- case semicontext of
                     Nothing -> return $ body start end
                     Just sc -> do v <- newVar
                                   return $ Compound "," [body start v, sc end v]
          return $ ((name, length params + 2), (head', body', NoCut))

dcgEmptyBodyp :: PParser (Expr -> Expr -> Expr)
dcgEmptyBodyp = acceptL ["."] *> pure dcgEmpty

dcgEmpty :: Expr -> Expr -> Expr
dcgEmpty start end = Compound "=" [start, end]

dcgBodyp :: PParser (Expr -> Expr -> Expr)
dcgBodyp = do acceptL ["{"]
              f1 <- orp
              acceptL ["}"]
              (do acceptL [","]
                  f2 <- dcgBodyp
                  return $ \start end -> Compound "," [f1, f2 start end]
                  ) <|> (acceptL ["."] *> pure (\start end -> Compound "," [f1, dcgEmpty start end]))
       <|> do f1 <- dcgCallp <|> dcgListp
              (do acceptL [","]
                  f2 <- dcgBodyp
                  var <- newVar
                  return $ \start end -> Compound "," [f1 start var, f2 var end]
                  ) <|> (acceptL ["."] *> pure f1)

dcgCallp = do head@(Compound name params) <- callp <|> parp
              return $ \start end -> Compound name (params ++ [start, end])

dcgListp = do t <- peekToken
              list <- listp <|> stringp
              checkList t list
              return $ \start end -> Compound "=" [start, addTail end list]

checkList _ (Compound "[]" []) = return ()
checkList t (Compound "." [_, b]) = checkList t b
checkList t _ = parsingError t "proper list"

addTail :: Expr -> Expr -> Expr
addTail end (Compound "[]" []) = end
addTail end (Compound "." [a, b]) = Compound "." [a, addTail end b]

operatorp :: [String] -> PParser Expr -> PParser Expr
operatorp ops subp = do e <- subp
                        es <- many (do op <- oneOfL ops
                                       e' <- subp
                                       return (content op, e'))
                        return $ foldl (\a (op, b) -> Compound op [a, b]) e es

hornp :: PParser Expr
hornp = operatorp [":-"] orp

orp :: PParser Expr
orp = operatorp [";", "!;"] ifp

ifp :: PParser Expr
ifp = operatorp ["->"] andp

andp :: PParser Expr
andp = operatorp [","] firstp

firstp = unifyp

unifyp :: PParser Expr
unifyp = operatorp ["on", "=", "\\=", "<", ">", "<=", ">=", "=.."] sump

sump :: PParser Expr
sump = operatorp ["+", "-"] termp

termp :: PParser Expr
termp = operatorp ["*", "/", "//", "%"] simplep

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
          return $ SymbolVar s

anonVarp :: PParser Expr
anonVarp = do t <- peekToken
              s@(c:cs) <- nextL
              unless (c == '_') $
                parsingError t "unused variable"
              newVar

newVar :: PParser Expr
newVar = do c <- lift get
            lift $ put (c+1)
            return $ SymbolVar ('_':'.':show c)

callp :: PParser Expr
callp = do name <- content <$> noneOfL ["(", ")", pEofStr]
           args <- argsp
           return $ Compound name args
    <|> do name <- identifierL
           args <- argsp <|> pure []
           return $ Compound name args

argsp :: PParser [Expr]
argsp = do acceptL ["("]
           args <- (:) <$> firstp <*> many (acceptL [","] *> firstp)
           acceptL [")"]
           return args
    <|> acceptL["(", ")"] *> pure []

listp :: PParser Expr
listp = emptylistp <|> nonemptylistp

emptylistp :: PParser Expr
emptylistp = acceptL ["[", "]"] *> pure (Compound "[]" [])

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
                          fst $ runState (parse (hornp <* acceptL [pEofStr]) (tokens++[pEof])) 1

parseClauses :: String -> Either [ParsingError] [Statement]
parseClauses code = do tokens <- lexCode code
                       fst $ runState (parse (programp [pEofStr]) (tokens++[pEof])) 1

pEofStr = "<EOF>"
pEof = LNToken pEofStr 0 0
