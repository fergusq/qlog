module Main where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import System.Console.Haskeline
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
            Right fs -> runInputT (programSettings fs) $ queryLoop fs

programSettings :: M.Map (String, Int) [Clause] -> Settings IO
programSettings fs = setComplete (completer fs) defaultSettings

completer :: M.Map (String, Int) [Clause] -> CompletionFunc IO
completer fs = completeWord Nothing " \t" . completeKeys . sort . nub . (++ builtinPredicates) . map fst $ M.keys fs
completeKeys keys key = return . map simpleCompletion $ filter (isPrefixOf key) keys

queryLoop :: M.Map (String, Int) [Clause] -> InputT IO ()
queryLoop fs = do input <- getInputLine "?- "
                  case input of
                    Nothing -> return ()
                    Just query ->
                      if null query
                        then queryLoop fs
                        else case parseExpression query of
                          Left error -> outputStrLn (show error) >> queryLoop fs
                          Right expr -> do let vars = reverse $ zip (reverse . nub $ searchVars [] expr) (map Variable [1..])
                                           let goal = eval fs [] expr
                                           results <- lift . L.uncons $ goal S.empty emptyState
                                           output vars results
                                           queryLoop fs

output :: [(String, Expr)] -> Maybe (Substitutions, L.ListT IO Substitutions) -> InputT IO ()
output vars Nothing = outputStrLn "Ei."
output vars (Just (state, results)) = outputStrLn "Kyllä." >> output' False vars state results

output' :: Bool -> [(String, Expr)] -> Substitutions -> L.ListT IO Substitutions -> InputT IO ()
output' outputAll vars (state@Substitutions { substitutions = ss }) n
  = do outputStr . intercalate ",\n" $ map (\(v, e) -> v ++ " = " ++ (show $ deepWalk state e)) vars
       results <- lift $ L.uncons n
       case results of
         Nothing -> outputStrLn ""
         Just (state, results) ->
           if outputAll
             then outputStrLn " ;" >> output' True vars state results
             else do
               cmd <- getInputChar " ? "
               case cmd of
                 Nothing -> outputStrLn ""
                 Just ';' -> output' False vars state results
                 Just 'k' -> output' True vars state results
                 Just _ -> outputStrLn ""
