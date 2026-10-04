{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Effects.Internal.Continuation (
  StateIO,
  stateIO,
  runStateIO,
  getIO,
  setIO,
  modifyIO,
  PromptTag,
  newPromptTag,
  prompt,
  control,
  control0,
) where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import GHC.Exts (PromptTag#, RealWorld, State#, TYPE, control0#, newPromptTag#, prompt#)
import GHC.IO (IO (..), unIO)

newtype StateIO s a = StateIO# (s -> IO# (# s, a #))
  deriving stock Functor

stateIO :: (s -> IO (s, a)) -> StateIO s a
stateIO k = stateIO# (\s -> unIO (k s))

stateIO# :: (s -> IO# (s, a)) -> StateIO s a
stateIO# k = StateIO# \s w -> case k s w of
  (# w', (s', x) #) -> (# w', (# s', x #) #)

runStateIO :: s -> StateIO s a -> IO (s, a)
runStateIO s m = IO (runStateIO# s m)

runStateIO# :: s -> StateIO s a -> IO# (s, a)
runStateIO# s (StateIO# m) w = case m s w of
  (# w', (# s', x #) #) -> (# w', (s', x) #)

getIO :: StateIO s s
getIO = stateIO (\s -> pure (s, s))

setIO :: s -> StateIO s ()
setIO s = modifyIO (const s)

modifyIO :: (s -> s) -> StateIO s ()
modifyIO f = stateIO (\s -> pure (f s, ()))

instance Applicative (StateIO s) where
  pure x = stateIO (\s -> pure (s, x))
  (<*>) = ap

instance Monad (StateIO s) where
  m >>= f = stateIO \s -> do
    (!s', x) <- runStateIO s m
    runStateIO s' (f x)

instance MonadIO (StateIO s) where
  liftIO m = stateIO (\s -> (s,) <$> m)

data PromptTag s a = MkPromptTag (PromptTag# (s, a))

newPromptTag :: StateIO s (PromptTag s a)
newPromptTag = StateIO# \s w -> case newPromptTag# w of
  (# w', tag #) -> (# w', (# s, MkPromptTag tag #) #)

prompt :: PromptTag s a -> StateIO s a -> StateIO s a
prompt (MkPromptTag tag) m = stateIO# \s -> prompt# tag (runStateIO# s m)

control :: PromptTag s a -> ((StateIO s b -> StateIO s a) -> StateIO s a) -> StateIO s b
control tag h = control0 tag (prompt tag . h)

-- NOTE: before 'control0', we expect the following call stack:
--   prompt tag | k | control0
-- now 'control0' captures the continuation 'k', drops the 'prompt' frame
-- the handler proceeds (pushed onto the stack), then call 'k' with some 'm'
--   handler-cont | k | m
-- NOTE: state threading for 'prompt' and 'control0'
-- 1. the handler 'h' receives the state (1) at the point of 'control0'
-- 2. handler proceeds until it invokes 'k' with an evolved state (2)
-- 3. 'm' evaluates in the context of 'k', producing an evolved state (3)
--    (potentially with all the exception handlers of 'k' in scope)
--    (this is the reason why 'm' is a computation and not a value)
-- 4. the continuation 'k' finally resumes, inheriting the state (3) and producing a state (4)
control0 :: PromptTag s a -> ((StateIO s b -> StateIO s a) -> StateIO s a) -> StateIO s b
control0 (MkPromptTag tag) h = StateIO# \s -> control0# tag \k -> runStateIO# s (h (wrapCont k))

type IO# :: TYPE r -> Type
type IO# a = State# RealWorld -> (# State# RealWorld, a #)

-- NOTE: the continuation to be wrapped should start with a bind
-- so whatever 's' passed in should get inherited by the whole continuation
wrapCont :: (IO# (# s, a #) -> IO# (s, b)) -> StateIO s a -> StateIO s b
wrapCont k (StateIO# r) = stateIO# \s -> k (r s)
