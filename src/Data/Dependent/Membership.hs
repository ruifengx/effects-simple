module Data.Dependent.Membership (
  Membership (Here, There),
  getMembership,
  unsafeCoerceMembership,
) where

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

type Membership :: k -> [k] -> Type
type role Membership nominal nominal
newtype Membership x xs = RawMembership Word

getMembership :: Membership x xs -> Int
getMembership (RawMembership n) = fromIntegral n

unsafeCoerceMembership :: Membership x xs -> Membership y ys
unsafeCoerceMembership (RawMembership n) = RawMembership n

pattern Here :: () => ys ~ x : xs => Membership x ys
pattern Here <- (viewMembership -> HereView) where Here = RawMembership 0

pattern There :: () => ys ~ y : xs => Membership x xs -> Membership x ys
pattern There m <- (viewMembership -> ThereView m) where There (RawMembership n) = RawMembership (n + 1)

type MembershipView :: k -> [k] -> Type
data MembershipView x xs where
  HereView :: MembershipView x (x : xs)
  ThereView :: Membership x xs -> MembershipView x (y : xs)

viewMembership :: Membership x xs -> MembershipView x xs
viewMembership (RawMembership 0) = unsafeCoerce HereView
viewMembership (RawMembership n) = unsafeCoerce (ThereView (RawMembership (n - 1)))
