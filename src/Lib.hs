module Lib where
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M

data Expr = Variable Int
          | Compound String [Expr]
          deriving (Eq)

instance Show Expr where
  show (Variable v) = "_" ++ show v
  show (Compound f []) = f
  show (Compound f as) = f ++ "(" ++ intercalate ", " (map show as) ++ ")"

type SMap = M.Map Int Expr

data State = State { substitutions :: SMap, counter :: Int }

addSubstitution :: Int -> Expr -> State -> State
addSubstitution v x state = state { substitutions = M.insert v x (substitutions state) }

substitute :: Int -> Expr -> Expr -> Expr
substitute v r expr@(Variable u) = if v == u then r else expr
substitute v r (Compound f as) = Compound f $ map (substitute v r) as

type Goal = State -> [State]

walk :: State -> Expr -> Expr
walk state x@(Variable v) = fromMaybe x $ walk state <$> M.lookup v (substitutions state)
walk _     x              = x

deepWalk :: State -> Expr -> Expr
deepWalk state x@(Variable v)    = fromMaybe x $ deepWalk state <$> M.lookup v (substitutions state)
deepWalk state x@(Compound f as) = Compound f $ map (deepWalk state) as
deepWalk _     x                 = x

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

infixr 4 #=

(#=) = unify

infixl 3 #|#
infixl 3 #&#

(#|#) :: Goal -> Goal -> Goal
(#|#) x y state = x state <|> y state

(#&#) :: Goal -> Goal -> Goal
(#&#) x y state = x state >>= y

callFresh :: (Expr -> Goal) -> Goal
callFresh f state = let c = counter state + 1
                        state' = state { counter = c }
                    in f (Variable c) state'

fresh vs = fresh' vs []

fresh' :: Int -> [Expr] -> ([Expr] -> Goal) -> Goal
fresh' 0 es f = f es
fresh' n es f = callFresh $ \e -> fresh' (n-1) (e:es) f
