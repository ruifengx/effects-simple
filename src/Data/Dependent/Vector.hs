module Data.Dependent.Vector (
  HVec (HNil, HCons),
  HSplitView (..),
  split,
) where

import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Unsafe.Coerce (unsafeCoerce)

import Data.Dependent.Membership
import Effects.Internal.Erasure

type HVec :: (k -> [k] -> Type) -> [k] -> Type
type role HVec representational nominal
newtype HVec f xs = RawHVec (Seq Any)

pattern HNil :: () => xs ~ '[] => HVec f xs
pattern HNil <- (viewHVec -> NilView) where HNil = RawHVec Seq.empty

pattern HCons :: () => ys ~ x : xs => f x xs -> HVec f xs -> HVec f ys
pattern HCons x xs <- (viewHVec -> ConsView x xs) where HCons = cons

{-# COMPLETE HNil, HCons #-}

cons :: f x xs -> HVec f xs -> HVec f (x : xs)
cons x (RawHVec xs) = RawHVec (toAny x :<| xs)

type HSplitView :: (k -> [k] -> Type) -> k -> [k] -> Type
data HSplitView f x xs = forall ys. HSplit
  { merge :: f x ys -> HVec f ys -> HVec f xs
  , this :: f x ys
  , rest :: HVec f ys
  }

split :: Membership x xs -> HVec f xs -> HSplitView f x xs
split n (RawHVec xs) = case Seq.splitAt (getMembership n) xs of
  (_, Empty) -> error "impossible: membership ensures existence"
  (ys, x :<| zs) ->
    let merge x' (RawHVec zs') = RawHVec (ys <> (toAny x' :<| zs'))
     in unsafeCoerce HSplit{merge, this = fromAny x, rest = RawHVec zs}

type HVecView :: (k -> [k] -> Type) -> [k] -> Type
data HVecView f xs where
  NilView :: HVecView f '[]
  ConsView :: f x xs -> HVec f xs -> HVecView f (x : xs)

viewHVec :: HVec f xs -> HVecView f xs
viewHVec (RawHVec Empty) = unsafeCoerce NilView
viewHVec (RawHVec (x :<| xs)) = unsafeCoerce (ConsView (fromAny x) (RawHVec xs))
