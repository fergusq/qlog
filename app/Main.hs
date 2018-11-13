module Main where

import Control.Monad

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import System.Environment
import System.Exit
import System.IO

import qualified ListT as L

import Lib
import Logic
import Parser

main :: IO ()
main = do [code] <- getArgs
          case compileClauses M.empty code of
            Left error -> print error >> exitFailure
            Right fs -> do queryLoop fs

queryLoop :: M.Map (String, Int) [(Expr, Expr)] -> IO ()
queryLoop fs = do putStr "?- "
                  hFlush stdout
                  query <- getLine
                  if null query
                   then queryLoop fs
                   else case parseExpression query of
                    Left error -> print error >> queryLoop fs
                    Right expr -> do let vars = map (Variable.(1+)) . nub $ searchVars [] expr
                                     let goal = eval fs [] expr
                                     results <- L.uncons $ goal S.empty emptyState
                                     output vars results
                                     queryLoop fs

output :: [Expr] -> Maybe (Substitutions, L.ListT IO Substitutions) -> IO ()
output vars Nothing = putStrLn "Ei."
output vars (Just (state, results)) = putStrLn "Kyllä." >> output' vars state results

output' :: [Expr] -> Substitutions -> L.ListT IO Substitutions -> IO ()
output' vars (state@Substitutions { substitutions = ss }) n
  = do forM_ (zip (reverse vars) ['X', 'Y', 'Z']) $ \(e, v) ->
         putStrLn $ (v:" = ") ++ show (deepWalk state e)
       results <- L.uncons n
       case results of
         Nothing -> return ()
         Just (state, results) -> do
           line <- getLine
           when (null line) $
             output' vars state results
