module Lib where

import qualified Data.Map as M
import Data.Maybe

import Logic
import Parser
import ParserCombinators

compileFacts :: String -> Either [ParsingError] (M.Map (String, Int) [(Expr, Expr)])
compileFacts code = do facts <- parseFacts code
                       return $ compileFacts' M.empty facts

compileFacts' factMap []                     = factMap
compileFacts' factMap ((signature, fact):as) = let facts = fromMaybe [] $ M.lookup signature factMap
                                                   factMap' = M.insert signature (facts++[fact]) factMap
                                               in compileFacts' factMap' as
