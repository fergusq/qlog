module Lib where

import Control.Monad

import qualified Data.Map as M
import Data.Maybe

import System.Directory (withCurrentDirectory)
import System.FilePath (takeDirectory) 

import Logic
import Parser
import ParserCombinators

type ClauseMap = M.Map (String, Int) [Clause]

compileFile :: ClauseMap -> String -> IO (Either [ParsingError] ClauseMap)
compileFile clauseMap filename = do code <- readFile filename
                                    withCurrentDirectory (takeDirectory filename) $
                                      compileClauses clauseMap code

compileClauses :: ClauseMap -> String -> IO (Either [ParsingError] ClauseMap)
compileClauses clauseMap code = bindMEither (compileClauses' clauseMap) $ parseClauses code

compileClauses' :: ClauseMap -> [Statement] -> IO (Either [ParsingError] ClauseMap)
compileClauses' clauseMap [] = return $ Right clauseMap
compileClauses' clauseMap (ClauseDeclaration (signature, clause) : as)
  = let clauses = fromMaybe [] $ M.lookup signature clauseMap
        clauseMap' = M.insert signature (clauses++[clause]) clauseMap
    in compileClauses' clauseMap' as
compileClauses' clauseMap (Directive list@(Compound "." _) : as)
  = do let files = exprToList list
       let filenames = map ((++".ql") . show) files
       clauseMap' <- foldM (\m f -> bindMEither (\m' -> compileFile m' f) m) (Right clauseMap) filenames
       bindMEither (flip compileClauses' as) clauseMap'

bindMEither :: (Monad m) => (a -> m (Either l b)) -> Either l a -> m (Either l b)
bindMEither f e = case e of
                    Left l -> return $ Left l
                    Right r -> f r
