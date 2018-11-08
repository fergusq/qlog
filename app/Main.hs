module Main where

import Control.Monad
import qualified Data.Map as M
import Lib

main :: IO ()
main = do print natF
          let (vars, goal) = sampleQuery
          output vars $ goal (State {substitutions=M.empty, counter=0})

output :: [Expr] -> [State] -> IO ()
output vars [] = putStrLn "Ei."
output vars states = putStrLn "Kyllä." >> output' vars states

output' :: [Expr] -> [State] -> IO ()
output' vars [] = putStrLn "."
output' vars (state@State { substitutions = ss }:n) = do forM_ (zip vars ['X', 'Y', 'Z']) $ \(e, v) ->
                                                           putStrLn $ (v:" = ") ++ show (deepWalk state e)
                                                         getLine
                                                         output' vars n

infixr 9 //
(//) = Compound

sampleQuery = ([Variable 1], eval (M.fromList natF) [] [] ("nat"//[Variable 1]))
--sampleQuery = ([Variable 1], callFresh nat)
--sampleQuery = ([Variable 1], callFresh $ \x -> summa ("s"//["s"//["0"//[]]]) ("s"//["s"//["0"//[]]]) x)

summa x y s = (x #= "0"//[] #&# y #= s) #|# callFresh (\z -> x #= "s"//[z] #&# summa z ("s"//[y]) s)

nat x = callFresh $ \y -> (x #= "0"//[]) #|# (nat y #&# x #= "s"//[y])

natF = [("nat", (";"//["="//[Variable 0, "0"//[]], ","//["nat"//[Variable 1], "="//[Variable 0, "s"//[Variable 1]]]], [0]))]
