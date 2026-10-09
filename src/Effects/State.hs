module Effects.State (
  State (..),
  get,
  set,
  runState,
) where

import Data.Dependent.Membership
import Effects.Monad

data State s :: Effect where
  Get :: State s s
  Set :: s -> State s ()

get :: Membership (State s) es -> Eff es s
get m = send m Get

set :: Membership (State s) es -> s -> Eff es ()
set m s = send m (Set s)

runState :: s -> Eff (State s : es) a -> Eff es a
runState s0 = interpretDirect s0 \s -> \case
  Get -> pure (s, s)
  Set s' -> pure (s', ())
