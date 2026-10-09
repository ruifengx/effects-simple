module Effects.Monad (
  Effect,
  Eff,
  unsafeRunIO,
  send,
  interpretDirect,
  interpretControl,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Kind (Type)

import Data.Dependent.Membership
import Data.Dependent.Vector
import Effects.Internal.Continuation

type Effect = Type -> Type

newtype Eff es a = RawEff (StateIO (Env es) a)
  deriving newtype (Functor, Applicative, Monad)

wrapEff :: (Env es -> IO (Env es, a)) -> Eff es a
wrapEff f = RawEff (stateIO f)

runEff :: Env es -> Eff es a -> IO (Env es, a)
runEff env (RawEff m) = runStateIO env m

unsafeRunIO :: IO a -> Eff es a
unsafeRunIO = coerce liftIO

send :: Membership e es -> e a -> Eff es a
send k m = wrapEff \env -> case split k env of
  HSplit merge (EDirect fun) ys -> do
    (ys', (s, x)) <- runEff ys (sendDirect fun m)
    pure (merge (EDirect fun{storage = s}) ys', x)
  HSplit merge (EControl ctl) ys -> do
    (ys', x) <- runEff ys (sendControl ctl m)
    pure (merge (EControl ctl) ys', x)

sendDirect :: DirectInfo e es s -> e a -> Eff es (s, a)
sendDirect Direct{storage, action} m = action storage m

sendControl :: ControlInfo e es r -> e a -> Eff es a
sendControl Control{promptTag, action} m = coerce (control promptTag) (action m)

pushFrame :: EffectInfo e es -> Eff (e : es) a -> Eff es a
pushFrame info m = wrapEff \env -> first (\(HCons _ env') -> env') <$> runEff (HCons info env) m

interpretDirect :: s -> (forall r. s -> e r -> Eff es (s, r)) -> Eff (e : es) a -> Eff es a
interpretDirect s0 h m = pushFrame (EDirect Direct{storage = s0, snapshot = pure, action = h}) m

interpretControl
  :: (forall r. r -> Eff es (g r))
  -> (forall b r. e b -> (Eff es b -> Eff es (g r)) -> Eff es (g r))
  -> Eff (e : es) a
  -> Eff es (g a)
interpretControl ret act m = do
  promptTag <- coerce newPromptTag
  coerce (prompt promptTag) do
    ret =<< pushFrame (EControl Control{promptTag, action = act}) m

type Env = HVec EffectInfo

data EffectInfo e es where
  EDirect :: {-# UNPACK #-} DirectInfo e es s -> EffectInfo e es
  EControl :: {-# UNPACK #-} ControlInfo e es r -> EffectInfo e es

data DirectInfo e es s = Direct
  { storage :: s
  , snapshot :: s -> IO s
  , action :: forall a. s -> e a -> Eff es (s, a)
  }

data ControlInfo e es r = Control
  { promptTag :: PromptTag (Env es) r
  , action :: forall a. e a -> (Eff es a -> Eff es r) -> Eff es r
  }
