module Main where

import Control.Monad
import qualified Data.Map as M
import Lib

main :: IO ()
main = output $ sampleQuery (State {substitutions=M.empty, counter=0})

output [] = putStrLn "Ei."
output (state@State { substitutions = ss }:n) = do putStrLn "Kyllä."
                                                   forM_ (M.toList ss) $ \(v, e) -> putStrLn (v ++ " = " ++ show (deepWalk state e))
                                                   getLine
                                                   output n

(//) = Compound

sampleQuery = callFresh nat

nat x = (x #= ("0"//[])) #|# (callFresh $ \y -> nat y #&# (x #= ("s"//[y])))
