module Effects.Internal.Erasure (
  Any,
  toAny,
  fromAny,
) where

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce
