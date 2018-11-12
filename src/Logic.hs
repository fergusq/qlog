module Logic where
import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum, chr)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

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

exprToStr :: Expr -> String
exprToStr e = fromMaybe ('(' : show e ++ ")") $ strExprToStr e

strExprToStr :: Expr -> Maybe String
strExprToStr (Compound "." [SymbolInt i, tail]) = strExprToStr tail >>= Just . (chr (fromInteger i) :)
strExprToStr (Compound "[]" []) = Just ""
strExprToStr _ = Nothing

type Clause = (Expr, Expr)

-- Substitutions

type SMap = M.Map Int Expr

data Substitutions = Substitutions { substitutions :: SMap, counter :: Int, gcTrigger :: Int } deriving Show

emptyState = Substitutions {
               substitutions = M.empty,
               counter = 0,
               gcTrigger = 10
             }

addSubstitution :: Int -> Expr -> Substitutions -> Substitutions
addSubstitution v x state = state { substitutions = M.insert v x (substitutions state) }

substitute :: Int -> Expr -> Expr -> Expr
substitute v r expr@(Variable u) = if v == u then r else expr
substitute v r (Compound f as) = Compound f $ map (substitute v r) as

-- Garbage collection

gc :: S.Set Int -> Substitutions -> Substitutions
gc vars state = if length (substitutions state) >= gcTrigger state then performGc vars state else state

performGc :: S.Set Int -> Substitutions -> Substitutions
performGc vars state@(Substitutions {substitutions = ss})
  = state { substitutions = gcdSs, gcTrigger = 2 * length gcdSs }
  where gcdSs = M.restrictKeys ss livingKeys
        livingKeys = findLivingVars ss vars S.empty

findLivingVars :: SMap -> S.Set Int -> S.Set Int -> S.Set Int
findLivingVars ss vars vvars = findLivingVars' ss (vars `S.union` vvars) . concatMap findVars $ lookupKeys ss (S.toList $ vars `S.difference` vvars)

findLivingVars' :: SMap -> S.Set Int -> [Int] -> S.Set Int
findLivingVars' ss vvars []     = vvars
findLivingVars' ss vvars (i:is) = let vvars' = findLivingVars ss (S.singleton i) vvars
                                  in findLivingVars' ss vvars' is

findVars :: Expr -> [Int]
findVars (Variable i)    = [i]
findVars (Compound _ as) = concatMap findVars as
findVars _               = []

lookupKeys :: SMap -> [Int] -> [Expr]
lookupKeys m ks = (flip M.lookup m <$> ks) >>= \x -> case x of { Nothing -> []; Just v -> [v] }

-- Goal

type Goal = S.Set Int -> Substitutions -> [(IO (), Substitutions)]

walk :: Substitutions -> Expr -> Expr
walk state x@(Variable v) = fromMaybe x $ walk state <$> M.lookup v (substitutions state)
walk _     x              = x

deepWalk :: Substitutions -> Expr -> Expr
deepWalk state x@(Variable v)    = fromMaybe x $ deepWalk state <$> M.lookup v (substitutions state)
deepWalk state x@(Compound f as) = Compound f $ map (deepWalk state) as
deepWalk _     x                 = x

-- Unification

unify :: Expr -> Expr -> Goal
unify a b vvs state = let a' = walk state a
                          b' = walk state b
                      in if a' == b'
                           then true vvs state
                           else case (a', b') of
                                  ((Variable v), _) -> true vvs $ addSubstitution v b state
                                  (_, (Variable v)) -> true vvs $ addSubstitution v a state
                                  (Compound v xs, Compound u ys) -> if v == u
                                                                      then unifyAll xs ys vvs state
                                                                      else false vvs state
                                  (_, _) -> false vvs state

unifyAll :: [Expr] -> [Expr] -> Goal
unifyAll []    [] = true
unifyAll (_:_) [] = false
unifyAll [] (_:_) = false
unifyAll (x:xs) (y:ys) = conj (unify x y) (unifyAll xs ys)

-- Conjunction and disjunction

conj :: Goal -> Goal -> Goal
conj x y vvs state = x vvs state >>= \(io, state') -> map (\(io', state'') -> (io >> io', state'')) $ y vvs (gc vvs state')

disj :: Goal -> Goal -> Goal
disj x y vvs state = x vvs state <|> y vvs state

cutDisj :: Goal -> Goal -> Goal
cutDisj x y vvs state = let left = x vvs state
                            failed = null left
                        in if failed
                             then y vvs state
                             else left

-- Operator aliases

infixr 4 #=
infixl 3 #|#
infixl 3 #&#

(#=) = unify
(#|#) = disj
(#&#) = conj

-- True and false

true :: Goal
true _ state = pure (return (), state)

false :: Goal
false _ _state = empty

-- callFresh and fresh

callFresh :: (Expr -> Goal) -> Goal
callFresh f vvs state = let c = counter state + 1
                            vvs' = S.insert c vvs
                            state' = state { counter = c }
                        in f (Variable c) vvs' state'

fresh :: Int -> ([Expr] -> Goal) -> Goal
fresh n = fresh' n []

fresh' :: Int -> [Expr] -> ([Expr] -> Goal) -> Goal
fresh' 0 es f = f es
fresh' n es f = callFresh $ \e -> fresh' (n-1) (e:es) f

-- Converting expressions to predicate functions

eval :: M.Map (String, Int) [Clause] -> [(Int, Expr)] -> Expr -> Goal
eval fs hvs e
  = fresh (length is) $ \is' -> let vs = hvs ++ zip is is' in eval' fs vs $ evalExpr vs e
  where is = nub $ searchVars (map fst hvs) e

searchVars :: [Int] -> Expr -> [Int]
searchVars ps (Variable i)
  | i `elem` ps = []
  | otherwise   = [i]
searchVars ps (Compound f as) = concatMap (searchVars ps) as
searchVars _  (SymbolInt _) = []

--tracedEval fs vs e state = let a = eval' fs vs e state in trace (show (deepWalk state e) ++ " <=> " ++ show (not $ null a)) a

eval' :: M.Map (String, Int) [Clause] -> [(Int, Expr)] -> Expr -> Goal
eval' fs vs e@(Variable _)                = \vvs state -> let e' = walk state e
                                                          in if e /= e'
                                                               then eval' fs vs e' vvs state
                                                               else error "sitomaton muuttuja"
eval' fs _  (SymbolInt _)                 = error "odotettiin funktoria"
eval' fs vs (Compound ";" [a, b])         = disj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "!;" [a, b])        = cutDisj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "," [a, b])         = conj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "=" [a, b])         = unify a b
eval' fs vs (Compound "\\+" [e])          = \vvs state -> if null (eval' fs vs e vvs state) then true vvs state else false vvs state
eval' fs vs (Compound "tosi" [])          = true
eval' fs vs (Compound "epätosi" [])       = false
eval' fs vs (Compound "on" [a, b])        = \vvs state -> unify a (evalMath state b) vvs state
eval' fs vs (Compound "muuttuja" [e])     = \vvs state -> case walk state e of { (Variable _) -> true vvs state; _ -> false vvs state }
eval' fs vs (Compound "kokonaisluku" [e]) = \vvs state -> case walk state e of { (SymbolInt _) -> true vvs state; _ -> false vvs state }
eval' fs vs (Compound "<" [a, b])         = \vvs state -> case map (walk state) [a, b] of
                                                            [SymbolInt i, SymbolInt j] -> if i < j then true vvs state else false vvs state
                                                            [Variable i, SymbolInt j] -> map ((,) (return ()) . flip (addSubstitution i) state . SymbolInt) [j-1,j-2..]
                                                            [SymbolInt i, Variable j] -> map ((,) (return ()) . flip (addSubstitution j) state . SymbolInt) [i+1,i+2..]
                                                            [Variable i, Variable j] -> map ((,) (return ()) . \x -> addSubstitution j (SymbolInt x) $
                                                                                                                     addSubstitution i (SymbolInt 0) state) [1..]
                                                            _ -> false vvs state
eval' fs vs (Compound ">" [a, b])         = eval' fs vs (Compound "<" [b, a])
eval' fs vs (Compound "<=" [a, b])        = disj (unify a b) (eval' fs vs (Compound "<" [a, b]))
eval' fs vs (Compound ">=" [a, b])        = eval' fs vs (Compound "<=" [b, a])
eval' fs vs (Compound "klausuuli" [h, b]) = \vvs state -> let e = walk state h
                                                          in case e of
                                                               (Compound f args) -> case M.lookup (f, length args) fs of
                                                                                      Just clauses -> unifyClauses e b clauses vvs state
                                                                                      Nothing -> empty
                                                               (Variable _) -> unifyClauses e b (concat $ M.elems fs) vvs state
                                                               _ -> false vvs state
eval' fs vs (Compound "näytä" [e])        = \_ state -> pure (putStr . show $ deepWalk state e, state)
eval' fs vs (Compound "tulosta" [e])      = \_ state -> pure (putStr . exprToStr $ deepWalk state e, state)
eval' fs vs (Compound "uusirivi" [])      = \_ state -> pure (putStr "\n", state)
eval' fs vs e@(Compound f   args)         = case M.lookup (f, length args) fs of
                                              Just clauses -> evalPredicate fs e clauses
                                              Nothing -> error ("määrittelemätön funktori "++f++"/"++show (length args))

evalPredicate :: M.Map (String, Int) [Clause] -> Expr -> [Clause] -> Goal
evalPredicate _  _    [] = false
evalPredicate fs pred cs = foldr1 disj $ map (evalPredicate' fs pred) cs

evalPredicate' fs pred (head, body)
  = fresh (length is) $ \is' -> let vs = zip is is' in conj (unify pred (evalExpr vs head)) $ eval fs vs body
  where is = nub $ searchVars [] head

evalExpr :: [(Int, Expr)] -> Expr -> Expr
evalExpr vs (Variable v)    = fromMaybe (error "sitomaton muuttuja") $ lookup v vs
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

unifyClauses :: Expr -> Expr -> [Clause] -> Goal
unifyClauses _ _ [] = false
unifyClauses h b cs = foldr1 disj $ map (unifyClause h b) cs

unifyClause :: Expr -> Expr -> Clause -> Goal
unifyClause head body (chead, cbody)
  = fresh (length is) $ \is' -> let vs = zip is is' in conj (unify head (evalExpr vs chead)) (unify body (evalExpr vs cbody))
  where is = nub $ searchVars [] chead ++ searchVars [] cbody
