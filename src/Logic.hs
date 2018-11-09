module Logic where
import Control.Applicative
import Data.Char (isAlphaNum)
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Debug.Trace

-- Expressions

data Expr = Variable Int
          | Compound String [Expr]
          | SymbolInt Integer
          deriving (Eq)

instance Show Expr where
  show (Variable v) = "_" ++ show v
  show (Compound "." [h, t]) = "[" ++ showExprList h t ++ "]"
  show (Compound f []) = f
  show (Compound f [a, b])
    | all (not . isAlphaNum) f = "(" ++ show a ++ " " ++ f ++ " " ++ show b ++ ")"
    | otherwise                = f ++ "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Compound f as) = f ++ "(" ++ intercalate ", " (map show as) ++ ")"
  show (SymbolInt i) = show i

showExprList :: Expr -> Expr -> String
showExprList h (Compound "[]" [])      = show h
showExprList h (Compound "." [h', t']) = show h ++ "," ++ showExprList h' t'
showExprList h t                       = show h ++ "|" ++ show t

-- State

type SMap = M.Map Int Expr

data Substitutions = Substitutions { substitutions :: SMap, counter :: Int } deriving Show

addSubstitution :: Int -> Expr -> Substitutions -> Substitutions
addSubstitution v x state = state { substitutions = M.insert v x (substitutions state) }

substitute :: Int -> Expr -> Expr -> Expr
substitute v r expr@(Variable u) = if v == u then r else expr
substitute v r (Compound f as) = Compound f $ map (substitute v r) as

-- Goal

type Goal = Substitutions -> [Substitutions]

walk :: Substitutions -> Expr -> Expr
walk state x@(Variable v) = fromMaybe x $ walk state <$> M.lookup v (substitutions state)
walk _     x              = x

deepWalk :: Substitutions -> Expr -> Expr
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

eval :: M.Map (String, Int) [(Expr, Expr)] -> [(Int, Expr)] -> Expr -> Goal
--eval fs hvs e state | trace ("fs="++show fs++" hvs="++show hvs++" e="++show e++" state="++show state) False = undefined
eval fs hvs e
  | null vs   = eval' fs hvs e
  | otherwise = fresh (length vs) (\vs' -> eval' fs (hvs ++ zip vs vs') e)
  where vs = nub $ searchVars (map fst hvs) e

searchVars :: [Int] -> Expr -> [Int]
searchVars ps (Variable i)
  | i `elem` ps = []
  | otherwise   = [i]
searchVars ps (Compound f as) = concatMap (searchVars ps) as
searchVars _  (SymbolInt _) = []

eval' :: M.Map (String, Int) [(Expr, Expr)] -> [(Int, Expr)] -> Expr -> Goal
--eval' fs vs e | trace ("' fs="++show fs++" vs="++show vs++" e="++show e) False = undefined
eval' fs _  (Variable _)           = error "odotettiin funktoria"
eval' fs _  (SymbolInt _)          = error "odotettiin funktoria"
eval' fs vs (Compound ";" [a, b])  = disj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "," [a, b])  = conj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "=" [a, b])  = unify (evalExpr vs a) (evalExpr vs b)
eval' fs vs (Compound "\\+" [e])   = \state -> if null (eval' fs vs e state) then [state] else []
eval' fs vs (Compound "on" [a, b]) = \state -> unify (evalExpr vs a) (evalMath state (evalExpr vs b)) state
eval' fs vs e@(Compound f   args)  = case M.lookup (f, length args) fs of
                                       Just clauses -> evalPredicate fs (evalExpr vs e) clauses
                                       Nothing -> error ("määrittelemätön funktori "++f++"/"++show (length args))

evalPredicate :: M.Map (String, Int) [(Expr, Expr)] -> Expr -> [(Expr, Expr)] -> Goal
evalPredicate _  pred []     = const []
evalPredicate fs pred (c:cs) = disj (evalPredicate' fs pred c) (evalPredicate fs pred cs)

evalPredicate' fs pred (head, body)
  | null vs   = conj (unify pred head) $ eval fs [] body
  | otherwise = fresh (length vs) $ \vs' -> conj (unify pred (evalExpr (zip vs vs') head)) $ eval fs (zip vs vs') body
  where vs = nub $ searchVars [] head

evalExpr :: [(Int, Expr)] -> Expr -> Expr
evalExpr vs (Variable v)    = fromMaybe (error "määrittelemätön muuttuja") $ lookup v vs
evalExpr vs (Compound f es) = Compound f (map (evalExpr vs) es)
evalExpr _  e               = e

evalMath :: Substitutions -> Expr -> Expr
evalMath state e = case walk state e of
                     t@(Compound f as) -> case (f, map (evalMath state) as) of
                                            ("+", [SymbolInt i, SymbolInt j]) -> SymbolInt (i+j)
                                            ("-", [SymbolInt i, SymbolInt j]) -> SymbolInt (i-j)
                                            ("*", [SymbolInt i, SymbolInt j]) -> SymbolInt (i*j)
                                            ("/", [SymbolInt i, SymbolInt j]) -> SymbolInt (i `div` j)
                                            ("%", [SymbolInt i, SymbolInt j]) -> SymbolInt (i `mod` j)
                                            _ -> t
                     (Variable _) -> error "sitomaton muuttuja"
                     t -> t
