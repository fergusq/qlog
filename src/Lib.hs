module Lib where

import Control.Monad

import qualified Data.Map as M
import Data.Maybe

import System.Directory (withCurrentDirectory)
import System.FilePath (takeDirectory) 

import Logic
import Parser
import ParserCombinators

data CompilerState = CS { clauseMap :: M.Map (String, Int) [Clause], loadedFiles :: [String] }
emptyCompilerState = CS { clauseMap = M.empty, loadedFiles = [] }

compileFile :: CompilerState -> String -> IO (Either [ParsingError] CompilerState)
compileFile state filename
  | filename `elem` loadedFiles state = return $ Right state
  | otherwise = do code <- readFile filename
                   let state' = state { loadedFiles = filename : loadedFiles state }
                   withCurrentDirectory (takeDirectory filename) $
                     compileClauses state' code

compileClauses :: CompilerState -> String -> IO (Either [ParsingError] CompilerState)
compileClauses state code = parseClauses code `bindMEither` compileClauses' state

compileClauses' :: CompilerState -> [Statement] -> IO (Either [ParsingError] CompilerState)
compileClauses' state [] = return $ Right state
compileClauses' state (ClauseDeclaration (signature, clause) : as)
  = let clauses = fromMaybe [] . M.lookup signature $ clauseMap state
        state' = state { clauseMap = M.insert signature (clauses++[clause]) $ clauseMap state }
    in compileClauses' state' as
compileClauses' state (Directive list@(Compound "." _) : as)
  = do let files = exprToList list
       let filenames = map ((++".ql") . show) files
       state' <- foldM (\m f -> bindMEither m $ \m' -> compileFile m' f) (Right state) filenames
       state' `bindMEither` flip compileClauses' as

bindMEither :: (Monad m) => Either l a -> (a -> m (Either l b)) -> m (Either l b)
bindMEither e f = case e of
                    Left l -> return $ Left l
                    Right r -> f r
