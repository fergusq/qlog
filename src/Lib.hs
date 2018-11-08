module Lib where
import Control.Applicative
import Data.Char (isAlphaNum)
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Debug.Trace

-- Expressions

data Expr = Variable Int
          | Compound String [Expr]
          deriving (Eq)

instance Show Expr where
  show (Variable v) = "_" ++ show v
  show (Compound f []) = f
  show (Compound f [a, b])
    | all (not . isAlphaNum) f = "(" ++ show a ++ " " ++ f ++ " " ++ show b ++ ")"
    | otherwise                = f ++ "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Compound f as) = f ++ "(" ++ intercalate ", " (map show as) ++ ")"

-- State

type SMap = M.Map Int Expr

data State = State { substitutions :: SMap, counter :: Int } deriving Show

addSubstitution :: Int -> Expr -> State -> State
addSubstitution v x state = state { substitutions = M.insert v x (substitutions state) }

substitute :: Int -> Expr -> Expr -> Expr
substitute v r expr@(Variable u) = if v == u then r else expr
substitute v r (Compound f as) = Compound f $ map (substitute v r) as

-- Goal

type Goal = State -> [State]

walk :: State -> Expr -> Expr
walk state x@(Variable v) = fromMaybe x $ walk state <$> M.lookup v (substitutions state)
walk _     x              = x

deepWalk :: State -> Expr -> Expr
deepWalk state x@(Variable v)    = fromMaybe x $ deepWalk state <$> M.lookup v (substitutions state)
deepWalk state x@(Compound f as) = Compound f $ map (deepWalk state) as
deepWalk _     x                 = x

-- Unification

unify :: Expr -> Expr -> Goal
unify a b state = let a' = walk state a
                      b' = walk state b
                  in if a' == b'
                       then [state]
                       else case (a', b') of
                              ((Variable v), _) -> [addSubstitution v b state]
                              (_, (Variable v)) -> [addSubstitution v a state]
                              (Compound v xs, Compound u ys) -> if v == u
                                                                  then unifyAll xs ys state
                                                                  else []
                              (_, _) -> []

unifyAll []    [] s = [s]
unifyAll (_:_) [] _ = []
unifyAll [] (_:_) _ = []
unifyAll (x:xs) (y:ys) state = unify x y state >>= unifyAll xs ys

-- Conjunction and disjunction

disj :: Goal -> Goal -> Goal
disj x y state = x state <|> y state

conj :: Goal -> Goal -> Goal
conj x y state = x state >>= y

-- Operator aliases

infixr 4 #=
infixl 3 #|#
infixl 3 #&#

(#=) = unify
(#|#) = disj
(#&#) = conj

-- callFresh and fresh

callFresh :: (Expr -> Goal) -> Goal
callFresh f state = let c = counter state + 1
                        state' = state { counter = c }
                    in f (Variable c) state'

fresh :: Int -> ([Expr] -> Goal) -> Goal
fresh n = fresh' n []

fresh' :: Int -> [Expr] -> ([Expr] -> Goal) -> Goal
fresh' 0 es f = f es
fresh' n es f = callFresh $ \e -> fresh' (n-1) (e:es) f

-- Converting expressions to predicate functions

eval :: M.Map String (Expr, [Int]) -> [Int] -> [Expr] -> Expr -> Goal
--eval fs ps as e state | trace ("fs="++show fs++" ps="++show ps++" as="++show as++" e="++show e++" state="++show state) False = undefined
eval fs ps as e
  | null vs   = eval' fs ps [] as e
  | otherwise = fresh (length vs) (\vs' -> eval' fs ps (zip vs vs') as e)
  where vs = nub $ searchVars ps e

searchVars :: [Int] -> Expr -> [Int]
searchVars ps (Variable i)
  | i `elem` ps = []
  | otherwise   = [i]
searchVars ps (Compound f as) = concatMap (searchVars ps) as

eval' :: M.Map String (Expr, [Int]) -> [Int] -> [(Int, Expr)] -> [Expr] -> Expr -> Goal
--eval' fs ps vs as e | trace ("' fs="++show fs++" ps="++show ps++" vs="++show vs++" as="++show as++" e="++show e) False = undefined
eval' fs _  _  _  (Variable _)          = error "expected predicate"
eval' fs ps vs as (Compound ";" [a, b]) = disj (eval' fs ps vs as a) (eval' fs ps vs as b)
eval' fs ps vs as (Compound "," [a, b]) = conj (eval' fs ps vs as a) (eval' fs ps vs as b)
eval' fs ps vs as (Compound "=" [a, b]) = unify (evalExpr ps vs as a) (evalExpr ps vs as b)
eval' fs ps vs as (Compound f   args)   = case M.lookup f fs of
                                            Just (body, parameters) -> eval fs parameters (map (evalExpr ps vs as) args) body
                                            Nothing -> error ("undefined predicate "++f)

evalExpr :: [Int] -> [(Int, Expr)] -> [Expr] -> Expr -> Expr
evalExpr ps vs as (Variable v)    = case elemIndex v ps of
                                      Just i  -> as !! i
                                      Nothing -> fromMaybe (error "undefined variable") $ lookup v vs
evalExpr ps vs as (Compound f es) = Compound f (map (evalExpr ps vs as) es)
