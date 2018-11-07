module Lib where
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M

data Expr = Variable String
          | Compound String [Expr]
          deriving (Show, Eq)

data State = State { substitutions :: M.Map String Expr, counter :: Int }

addSubstitution :: String -> Expr -> State -> State
addSubstitution v x state = state { substitutions = M.insert v x (substitutions state) }

substitute :: String -> Expr -> Expr -> Expr
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

(#=) = unify

(#|#) :: Goal -> Goal -> Goal
(#|#) x y state = x state <|> y state

(#&#) :: Goal -> Goal -> Goal
(#&#) x y state = x state >>= y

callFresh :: (Expr -> Goal) -> Goal
callFresh f state = let c = counter state + 1
                        state' = state { counter = c }
                    in f (Variable ('_':show c)) state'

fresh vs = fresh' vs []

fresh' :: Int -> [Expr] -> ([Expr] -> Goal) -> Goal
fresh' 0 es f = f es
fresh' n es f = callFresh $ \e -> fresh' (n-1) (e:es) f
