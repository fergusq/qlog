module Lib where

import qualified Data.Map as M
import Data.Maybe

import Logic
import Parser
import ParserCombinators

compileClauses :: String -> Either [ParsingError] (M.Map (String, Int) [(Expr, Expr)])
compileClauses code = do clauses <- parseClauses code
                         return $ compileClauses' M.empty clauses

compileClauses' clauseMap []                       = clauseMap
compileClauses' clauseMap ((signature, clause):as) = let clauses = fromMaybe [] $ M.lookup signature clauseMap
                                                         clauseMap' = M.insert signature (clauses++[clause]) clauseMap
                                                     in compileClauses' clauseMap' as
