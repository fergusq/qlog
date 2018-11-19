module Logic where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum, chr)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import qualified ListT as L

import Debug.Trace

-- Expressions

data Expr = Variable Int
          | Compound String [Expr]
          | SymbolInt Integer
          | SymbolVar String
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
  show (SymbolVar v) = v

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

listToExpr :: [Expr] -> Expr
listToExpr = foldr (\a b -> Compound "." [a, b]) (Compound "[]" [])

exprToList :: Expr -> [Expr]
exprToList (Compound "[]" [])    = []
exprToList (Compound "." [h, t]) = h : exprToList t
exprToList _                     = error "odotettiin listaa"

type Clause = (Expr, Expr, Mode)
data Mode = NoCut | Cut

instance Show Mode where
  show NoCut = "."
  show Cut = "!."

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

type Goal = S.Set Int -> Substitutions -> L.ListT IO Substitutions

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
                                  (Compound v xs, Compound u ys) -> if v == u && length xs == length ys
                                                                      then foldr conj true (zipWith unify xs ys) vvs state
                                                                      else false vvs state
                                  (_, _) -> false vvs state

partialUnify :: Expr -> Expr -> Goal
partialUnify a b vvs state = let a' = walk state a
                                 b' = walk state b
                             in if a' == b'
                                  then true vvs state
                                  else case (a', b') of
                                         ((Variable v), _) -> true vvs $ addSubstitution v b state
                                         (_, (Variable v)) -> true vvs $ addSubstitution v a state
                                         (Compound v xs, Compound u ys) -> if v == u && length xs == length ys
                                                                             then foldr conj true (zipWith partialUnify xs ys) vvs state
                                                                             else true vvs state
                                         (_, _) -> true vvs state

-- Conjunction and disjunction

conj :: Goal -> Goal -> Goal
conj x y vvs state = x vvs state >>= \state' -> y vvs (gc vvs state')

disj :: Goal -> Goal -> Goal
disj x y vvs state = x vvs state <|> y vvs state

cutDisj :: Goal -> Goal -> Goal
cutDisj x y vvs state = do let left = x vvs state
                           failed <- liftIO $ L.null left
                           if failed
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
true _ state = pure state

false :: Goal
false _ _state = empty

boolToGoal :: Bool -> Goal
boolToGoal True = true
boolToGoal False = false

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

eval :: M.Map (String, Int) [Clause] -> [(String, Expr)] -> Expr -> Goal
eval fs hvs e
  = fresh (length is) $ \is' -> let vs = hvs ++ zip is is' in eval' fs vs $ evalExpr vs e
  where is = nub $ searchVars (map fst hvs) e

searchVars :: [String] -> Expr -> [String]
searchVars ps (SymbolVar i)
  | i `elem` ps = []
  | otherwise   = [i]
searchVars ps (Compound f as) = concatMap (searchVars ps) as
searchVars _  (SymbolInt _) = []
searchVars _  (Variable _) = error "määrittelemätön muuttuja (!!)"

--tracedEval fs vs e state = let a = eval' fs vs e state in trace (show (deepWalk state e) ++ " <=> " ++ show (not $ null a)) a

builtinPredicates :: [String]
builtinPredicates = [
  ";", "!;", ",", "=", "\\+", "=..",
  "kaikille", "kaikki", "yksi",
  "tosi", "epätosi", "on", "muuttuja", "kokonaisluku",
  "<", ">", "<=", ">=", "välillä",
  "klausuuli", "listaus",
  "näytä", "tulosta", "rivinvaihto", "sisennys"]

eval' :: M.Map (String, Int) [Clause] -> [(String, Expr)] -> Expr -> Goal
eval' fs vs e@(Variable _)                = \vvs state -> let e' = walk state e
                                                          in if e /= e'
                                                               then eval' fs vs e' vvs state
                                                               else error "sitomaton muuttuja"
eval' fs _  (SymbolInt _)                 = error "odotettiin funktoria"
eval' fs _  (SymbolVar _)                 = error "määrittelemätön muuttuja (!)"
eval' fs vs (Compound ";" [a, b])         = disj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "!;" [a, b])        = cutDisj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "," [a, b])         = conj (eval' fs vs a) (eval' fs vs b)
eval' fs vs (Compound "=" [a, b])         = unify a b
eval' fs vs (Compound "~=" [a, b])        = partialUnify a b
eval' fs vs (Compound "\\+" [e])          = \vvs state -> do c <- liftIO (L.null (eval' fs vs e vvs state))
                                                             boolToGoal c vvs state
eval' fs vs (Compound "kaikille" [a, b])  = \vvs state -> do cs <- liftIO . L.toReverseList $
                                                                     do state' <- eval' fs vs a vvs state
                                                                        c <- liftIO (L.null (eval' fs vs b vvs state'))
                                                                        return $ not c
                                                             boolToGoal (all id cs) vvs state
eval' fs vs (Compound "kaikki" [t, g, b]) = \vvs state -> do cs <- liftIO . L.toList $
                                                                     do state' <- eval' fs vs g vvs state
                                                                        return $ deepWalk state' t
                                                             unify b (listToExpr cs) vvs state
eval' fs vs (Compound "yksi" [g])         = \vvs state -> do head <- liftIO . L.uncons $  eval' fs vs g vvs state
                                                             case head of
                                                               Nothing -> false vvs state
                                                               Just (state', _) -> pure state'
eval' fs vs (Compound "tosi" [])          = true
eval' fs vs (Compound "epätosi" [])       = false
eval' fs vs (Compound "on" [a, b])        = \vvs state -> unify a (evalMath state b) vvs state
eval' fs vs (Compound "muuttuja" [e])     = \vvs state -> case walk state e of { (Variable _) -> true vvs state; _ -> false vvs state }
eval' fs vs (Compound "kokonaisluku" [e]) = \vvs state -> case walk state e of { (SymbolInt _) -> true vvs state; _ -> false vvs state }
eval' fs vs (Compound "<" [a, b])         = \vvs state -> case map (walk state) [a, b] of
                                                            [SymbolInt i, SymbolInt j] -> boolToGoal (i < j) vvs state
                                                            [Variable i, SymbolInt j] -> intListToStream i [j-1,j-2..] state
                                                            [SymbolInt i, Variable j] -> intListToStream j [i+1,i+2..] state
                                                            [Variable i, Variable j] -> intListToStream j [1..] $ addSubstitution i (SymbolInt 0) state
                                                            _ -> evalUserPredicate fs vs (Compound "<" [a, b]) vvs state
eval' fs vs (Compound ">" [a, b])         = eval' fs vs (Compound "<" [b, a])
eval' fs vs (Compound "<=" [a, b])        = disj (unify a b) (eval' fs vs (Compound "<" [a, b]))
eval' fs vs (Compound ">=" [a, b])        = eval' fs vs (Compound "<=" [b, a])
eval' fs vs (Compound "välillä" [a, b, e])= \vvs state -> case map (walk state) [a, e, b] of
                                                            [SymbolInt i, SymbolInt j, SymbolInt k] -> boolToGoal (i <= j && j <= k) vvs state
                                                            [SymbolInt i, Variable j, SymbolInt k] ->
                                                              if i <= k then intListToStream j [i..k] state else false vvs state
                                                            [SymbolInt i, SymbolInt j, Variable k] ->
                                                              if i <= j then intListToStream k [j..] state else false vvs state
                                                            [Variable i, SymbolInt j, SymbolInt k] ->
                                                              if j <= k then intListToStream i [j,j-1..] state else false vvs state
                                                            [SymbolInt i, Variable j, Variable k] ->
                                                              intListToStream k [i..] $ addSubstitution j (SymbolInt i) state
                                                            [Variable i, Variable j, SymbolInt k] ->
                                                              intListToStream i [k,k-1..] $ addSubstitution j (SymbolInt k) state
                                                            [Variable i, SymbolInt j, Variable k] ->
                                                              intListToStream k [j..] $ addSubstitution i (SymbolInt j) state
                                                            [Variable i, Variable j, Variable k] ->
                                                              intListToStream k [0..] . addSubstitution i (SymbolInt 0) $ addSubstitution j (SymbolInt 0) state
                                                            _ -> false vvs state
eval' fs vs (Compound "klausuuli" [h, b]) = \vvs state -> let e = walk state h
                                                          in case e of
                                                               (Compound f args) -> case M.lookup (f, length args) fs of
                                                                                      Just clauses -> unifyClauses e b clauses vvs state
                                                                                      Nothing -> empty
                                                               (Variable _) -> unifyClauses e b (concat $ M.elems fs) vvs state
                                                               _ -> false vvs state
eval' fs vs (Compound "listaus" [p])      = \vvs state ->
  case deepWalk state p of
    Compound "/" [Compound f [], SymbolInt a] -> case M.lookup (f, fromInteger a) fs of
                                                   Just cs -> do liftIO $
                                                                   forM_ cs $ \(head, body, mode) ->
                                                                     putStrLn $ (show $ deepWalk state head) ++ " :- "
                                                                                                             ++ (show $ deepWalk state body)
                                                                                                             ++ show mode
                                                                 true vvs state
                                                   Nothing -> false vvs state
    Compound "//" [Compound f [], SymbolInt a] -> eval' fs vs (Compound "listaus" [Compound "/" [Compound f [], SymbolInt $ a+2]]) vvs state
    _ -> false vvs state
eval' fs vs (Compound "=.." [p, l])       = \vvs state -> let e = walk state p
                                                          in case e of
                                                               (Compound f args) ->
                                                                 let l' = Compound "." [Compound f [], listToExpr args]
                                                                 in unify l l' vvs state
                                                               (Variable _) ->
                                                                 case deepWalk state l of
                                                                   (Compound "." [Compound f [], args]) ->
                                                                     let p' = Compound f (exprToList args)
                                                                     in unify p p' vvs state
                                                                   _ -> false vvs state
                                                               _ -> false vvs state
eval' fs vs (Compound "näytä" [e])        = \_ state -> liftIO (putStr . show $ deepWalk state e) >> pure state
eval' fs vs (Compound "tulosta" [e])      = \_ state -> liftIO (putStr . exprToStr $ deepWalk state e) >> pure state
eval' fs vs (Compound "sisennys" [e])     = \vvs state -> case walk state e of
                                                            SymbolInt i -> liftIO (putStr $ replicate (fromInteger i) ' ') >> pure state
                                                            _ -> false vvs state
eval' fs vs (Compound "rivinvaihto" [])   = \_ state -> liftIO (putStr "\n") >> pure state
eval' fs vs (Compound "rv" [])            = eval' fs vs (Compound "rivinvaihto" [])
eval' fs vs e@(Compound _ _)              = evalUserPredicate fs vs e

evalUserPredicate :: M.Map (String, Int) [Clause] -> [(String, Expr)] -> Expr -> Goal
evalUserPredicate fs vs e@(Compound f args) = case M.lookup (f, length args) fs of
                                                Just clauses -> evalPredicate fs e clauses
                                                Nothing -> error ("määrittelemätön funktori "++f++"/"++show (length args))

listToStream :: Int -> [Expr] -> Substitutions -> L.ListT IO Substitutions
listToStream var exprs state = L.fromFoldable $ map (flip (addSubstitution var) state) exprs

intListToStream :: Int -> [Integer] -> Substitutions -> L.ListT IO Substitutions
intListToStream var ints = listToStream var $ map SymbolInt ints

evalPredicate :: M.Map (String, Int) [Clause] -> Expr -> [Clause] -> Goal
evalPredicate _  _    [] = false
evalPredicate fs pred (c@(_, _, mode):cs) = disj' (evalPredicate' fs pred c) (evalPredicate fs pred cs)
  where disj' = case mode of
                  NoCut -> disj
                  Cut -> cutDisj

evalPredicate' fs pred (head, body, _)
  = fresh (length is) $ \is' -> let vs = zip is is' in conj (unify pred (evalExpr vs head)) $ eval fs vs body
  where is = nub $ searchVars [] head

evalExpr :: [(String, Expr)] -> Expr -> Expr
evalExpr vs (SymbolVar v)   = fromMaybe (error "määrittelemätön muuttuja") $ lookup v vs
evalExpr vs (Compound f es) = Compound f (map (evalExpr vs) es)
evalExpr _  e               = e

evalMath :: Substitutions -> Expr -> Expr
evalMath state e = case walk state e of
                     t@(Compound f as) -> case (f, map (evalMath state) as) of
                                            ("+", [SymbolInt i, SymbolInt j]) -> SymbolInt (i+j)
                                            ("-", [SymbolInt i, SymbolInt j]) -> SymbolInt (i-j)
                                            ("*", [SymbolInt i, SymbolInt j]) -> SymbolInt (i*j)
                                            ("/", [SymbolInt i, SymbolInt j]) -> SymbolInt (i `div` j)
                                            ("mod", [SymbolInt i, SymbolInt j]) -> SymbolInt (i `mod` j)
                                            _ -> t
                     (Variable _) -> error "sitomaton muuttuja"
                     t -> t

unifyClauses :: Expr -> Expr -> [Clause] -> Goal
unifyClauses _ _ [] = false
unifyClauses h b cs = foldr1 disj $ map (unifyClause h b) cs

unifyClause :: Expr -> Expr -> Clause -> Goal
unifyClause head body (chead, cbody, _)
  = fresh (length is) $ \is' -> let vs = zip is is' in conj (unify head (evalExpr vs chead)) (unify body (evalExpr vs cbody))
  where is = nub $ searchVars [] chead ++ searchVars [] cbody
