module Main where

import Control.Monad
import qualified Data.Map as M
import Lib

main :: IO ()
main = output $ sampleQuery (State {substitutions=M.empty, counter=0})

output [] = putStrLn "Ei."
output (state@State { substitutions = ss }:n) = do putStrLn "Kyllä."
                                                   putStrLn $ "X = " ++ show (deepWalk state (Variable 1))
                                                   getLine
                                                   output n

infixr 9 //
(//) = Compound

sampleQuery = callFresh nat --callFresh $ \x -> summa ("s"//["s"//["0"//[]]]) ("s"//["s"//["0"//[]]]) x

summa x y s = (x #= "0"//[] #&# y #= s) #|# callFresh (\z -> x #= "s"//[z] #&# summa z ("s"//[y]) s)

nat x = x #= "0"//[] #|# callFresh (\y -> nat y #&# x #= "s"//[y])
