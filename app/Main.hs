module Main where

import Control.Monad

import Data.List
import qualified Data.Map as M

import System.Environment
import System.Exit
import System.IO

import Lib
import Logic
import Parser

main :: IO ()
main = do [code] <- getArgs
          case compileFacts code of
            Left error -> print error >> exitFailure
            Right fs -> do queryLoop fs

queryLoop :: M.Map (String, Int) [(Expr, Expr)] -> IO ()
queryLoop fs = do putStr "?- "
                  hFlush stdout
                  query <- getLine
                  if null query
                   then queryLoop fs
                   else case parseExpression query of
                    Left error -> print error >> exitFailure
                    Right expr -> do let vars = map (Variable.(1+)) . nub $ searchVars [] expr
                                     let goal = eval fs [] expr
                                     output vars $ goal (Substitutions {substitutions=M.empty, counter=0})
                                     queryLoop fs

output :: [Expr] -> [Substitutions] -> IO ()
output vars [] = putStrLn "Ei."
output vars states = putStrLn "Kyllä." >> output' vars states

output' :: [Expr] -> [Substitutions] -> IO ()
output' vars [] = putStrLn "."
output' vars (state@Substitutions { substitutions = ss }:n)
  = do forM_ (zip vars ['X', 'Y', 'Z']) $ \(e, v) ->
         putStrLn $ (v:" = ") ++ show (deepWalk state e)
       unless (null n) $ do
         line <- getLine
         when (null line) $
           output' vars n
