module ParserCombinators where

import Data.Char
import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

-- | ParsingError contains strings representing the expected and actual values of the erronous token alongside with its line and column numbers

data ParsingError = ParsingError String String Int Int

instance Show ParsingError where
    show (ParsingError actual expected l c) = "Syntax error at " ++ show l ++ ":" ++ show c ++ ": unexpected token " ++ actual ++ ", expected " ++ expected

-- | ParserT monad is a function that, given a list of tokens, returns a parsed element and a new list of tokens (that shuld be a suffix of the original list)

newtype ParserT t m r = ParserT { runParserT :: [t] -> m (Either [ParsingError] ([t], r)) }

instance (Monad m) => Monad (ParserT t m) where
    return x = ParserT $ \l -> return $ Right (l, x)
    p >>= f = ParserT $ \l -> do e <- runParserT p l
                                 case e of
                                     Right (l', r) -> runParserT (f r) l'
                                     Left ms -> return (Left ms)

instance (Monad m) => Functor (ParserT t m) where
    fmap = liftM

instance (Monad m) => Applicative (ParserT t m) where
    pure = return
    (<*>) = ap

instance MonadTrans (ParserT t) where
    lift i = ParserT $ \l -> do r <- i
                                return $ Right (l, r)

instance (Monad m) => Alternative (ParserT t m) where

    -- empty :: ParserT t m r
    empty = ParserT $ \l -> return $ Left []

    -- (<|>) :: ParserT t m r -> ParserT t m r -> ParserT t m r
    a <|> b = ParserT $ \l -> do e <- runParserT a l
                                 case e of
                                     Right p -> return $ Right p
                                     Left ms -> do e' <- runParserT b l
                                                   case e' of
                                                       Left ms' -> return $ Left (ms++ms')
                                                       Right p -> return $ Right p

    -- some :: ParserT t m r -> ParserT t m [r]
    some a = do x <- a
                xs <- many a
                return (x:xs)

    -- many :: ParserT t m r -> ParserT t m [r]
    many a = ParserT $ \l -> do e <- runParserT a l
                                case e of
                                    Right (l', r) -> runParserT (many a >>= \r' -> return (r:r')) l'
                                    Left _ -> return $ Right (l, [])

-- | nothing eats zero tokens and return success

nothing :: (Monad m, Monoid r) => ParserT t m r
nothing = return mempty

-- | nextToken eats a tokens and returns it

nextToken :: (Monad m) => ParserT t m t
nextToken = ParserT $ \(t:ts) -> return $ Right (ts, t)

-- peekToken returns the next token without eating it

peekToken :: (Monad m) => ParserT t m t
peekToken = ParserT $ \ts@(t:_) -> return $ Right (ts, t)

-- | parsingError causes parsing to fail with the given error

parsingError :: (Monad m, Token t) => t -> String -> ParserT t m r
parsingError actual expected = ParserT $ \_ -> return (Left [ParsingError (show actual) expected line column])
    where (line, column) = location actual

-- | parse runs the parser and ignores the leftover tokens

parse :: (Monad m) => ParserT t m r -> [t] -> m (Either [ParsingError] r)
parse parser tokens = do result <- runParserT parser tokens
                         return $ do (_, r) <- result
                                     return r

-- | Token is anything that has a location

class (Show t) => Token t where
    location :: t -> (Int, Int)

-- | AcceptToken is a function that can either accept or reject a token, and its string representation

data AcceptToken t = AcceptToken { accepts :: t -> Bool, strRep :: String }

instance Show (AcceptToken t) where
    show = strRep

-- | A simple acceptor that accepts tokens that equal to the given token

acceptor :: (Token t, Eq t) => t -> AcceptToken t
acceptor c = AcceptToken { accepts = (c ==), strRep = show c }

accept :: (Token t, Monad m) => [AcceptToken t] -> ParserT t m ()
accept []     = return ()
accept (c:cs) = do t <- nextToken
                   if c `accepts` t
                       then accept cs
                       else parsingError t (show c)

acceptT :: (Token t, Eq t, Monad m) => [t] -> ParserT t m ()
acceptT = accept . (acceptor <$>)

nextWhile :: (Monad m) => (t -> Bool) -> ParserT t m [t]
nextWhile f = do t <- peekToken
                 if f t
                     then do nextToken
                             ts <- nextWhile f
                             return (t:ts)
                     else return []

wordChar :: Char -> Bool
wordChar c = isAlphaNum c || (c `elem` "_'")

wordCharToken :: LNToken Char -> Bool
wordCharToken (LNToken c _ _) = wordChar c

identifier :: (Monad m) => ParserT (LNToken Char) m (LNToken String)
identifier = lnToken $ do t <- peekToken
                          ident <- nextWhile wordCharToken
                          when (length ident == 0) $
                              parsingError t "identifier character"
                          return $ ident

keyword :: (Monad m) => String -> ParserT (LNToken Char) m ()
keyword kw = do t <- peekToken
                word <- content <$> identifier
                unless (word == kw) $
                    parsingError t kw

oneOf :: (Token t, Monad m) => [AcceptToken t] -> ParserT t m t
oneOf as = do t <- nextToken
              unless (any (`accepts` t) as) $
                  parsingError t ("one of " ++ show as)
              return t

oneOfT :: (Token t, Eq t, Monad m) => [t] -> ParserT t m t
oneOfT = oneOf . (acceptor <$>)

noneOf :: (Token t, Monad m) => [AcceptToken t] -> ParserT t m t
noneOf as = do t <- nextToken
               when (any (`accepts` t) as) $
                   parsingError t ("one of " ++ show as)
               return t

noneOfT :: (Token t, Eq t, Monad m) => [t] -> ParserT t m t
noneOfT = noneOf . (acceptor <$>)

space :: (Monad m) => ParserT (LNToken Char) m (LNToken Char)
space = oneOfL " \t\n\r"

spaces :: (Monad m) => ParserT (LNToken Char) m (LNToken String)
spaces = lnToken $ many space

-- | LNToken is a token that contains the line number and the column number

data LNToken t = LNToken t Int Int

-- | Returns the token inside LNToken

content :: LNToken t -> t
content (LNToken t _ _) = t

instance (Show t) => Token (LNToken t) where
    location (LNToken _ l c) = (l, c)

instance (Show t) => Show (LNToken t) where
    show (LNToken s ln co) = show s ++ " (at " ++ show ln ++ ":" ++ show co ++ ")"

instance Functor LNToken where
    fmap f (LNToken t c l) = LNToken (f t) c l

lnToken :: (Show t, Monad m) => ParserT (LNToken t) m [LNToken t] -> ParserT (LNToken t) m (LNToken [t])
lnToken p = do lnts <- p
               case lnts of
                   (t:_) -> let (l, c) = location t
                            in return $ LNToken (map (\(LNToken t _ _) -> t) lnts) l c
                   [] -> do t <- peekToken
                            let (l, c) = location t
                            return $ LNToken [] l c

ltAcceptor :: (Show t, Eq t) => t -> AcceptToken (LNToken t)
ltAcceptor s = AcceptToken { accepts = \(LNToken t _ _) -> s == t, strRep = show s }

acceptL :: (Show t, Eq t, Monad m) => [t] -> ParserT (LNToken t) m (LNToken [t])
acceptL ts = do t <- peekToken
                accept (ltAcceptor <$> ts)
                return (ts <$ t)

oneOfL :: (Show t, Eq t, Monad m) => [t] -> ParserT (LNToken t) m (LNToken t)
oneOfL = oneOf . (ltAcceptor <$>)

noneOfL :: (Show t, Eq t, Monad m) => [t] -> ParserT (LNToken t) m (LNToken t)
noneOfL = noneOf . (ltAcceptor <$>)

nextL :: (Monad m) => ParserT (LNToken t) m t
nextL = do (LNToken t _ _) <- nextToken
           return t

identifierL :: (Monad m) => ParserT (LNToken String) m String
identifierL = do t <- peekToken
                 s <- nextL
                 unless (all wordChar s) $
                     parsingError t "identifier"
                 return s
