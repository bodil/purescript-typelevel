module Data.Typelevel.Num.Sets where

import Prelude
import Data.Typelevel.Num.Reps (type (:*), D1, D0, D9, D8, D7, D6, D5, D4, D3, D2)
import Data.Typelevel.Undefined (undefined)
import Type.Proxy (Proxy)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

class Nat n where
  toInt :: n -> Int

-- | Sugar for `toInt`, providing a more intuitive type
toInt' :: forall n. Nat n => Proxy n -> Int
toInt' _ = toInt (undefined :: n)

class Nat n <= Pos n

instance natD0 :: Nat D0 where toInt _ = 0
instance natD1 :: Nat D1 where toInt _ = 1
instance natD2 :: Nat D2 where toInt _ = 2
instance natD3 :: Nat D3 where toInt _ = 3
instance natD4 :: Nat D4 where toInt _ = 4
instance natD5 :: Nat D5 where toInt _ = 5
instance natD6 :: Nat D6 where toInt _ = 6
instance natD7 :: Nat D7 where toInt _ = 7
instance natD8 :: Nat D8 where toInt _ = 8
instance natD9 :: Nat D9 where toInt _ = 9

instance posNatD0 :: Pos x => Nat (x :* D0) where toInt n = subLastDec n
instance posNatD1 :: Pos x => Nat (x :* D1) where toInt n = subLastDec n + 1
instance posNatD2 :: Pos x => Nat (x :* D2) where toInt n = subLastDec n + 2
instance posNatD3 :: Pos x => Nat (x :* D3) where toInt n = subLastDec n + 3
instance posNatD4 :: Pos x => Nat (x :* D4) where toInt n = subLastDec n + 4
instance posNatD5 :: Pos x => Nat (x :* D5) where toInt n = subLastDec n + 5
instance posNatD6 :: Pos x => Nat (x :* D6) where toInt n = subLastDec n + 6
instance posNatD7 :: Pos x => Nat (x :* D7) where toInt n = subLastDec n + 7
instance posNatD8 :: Pos x => Nat (x :* D8) where toInt n = subLastDec n + 8
instance posNatD9 :: Pos x => Nat (x :* D9) where toInt n = subLastDec n + 9

instance posD1 :: Pos D1
instance posD2 :: Pos D2
instance posD3 :: Pos D3
instance posD4 :: Pos D4
instance posD5 :: Pos D5
instance posD6 :: Pos D6
instance posD7 :: Pos D7
instance posD8 :: Pos D8
instance posD9 :: Pos D9

instance posPosD0 :: Pos x => Pos (x :* D0)
instance posPosD1 :: Pos x => Pos (x :* D1)
instance posPosD2 :: Pos x => Pos (x :* D2)
instance posPosD3 :: Pos x => Pos (x :* D3)
instance posPosD4 :: Pos x => Pos (x :* D4)
instance posPosD5 :: Pos x => Pos (x :* D5)
instance posPosD6 :: Pos x => Pos (x :* D6)
instance posPosD7 :: Pos x => Pos (x :* D7)
instance posPosD8 :: Pos x => Pos (x :* D8)
instance posPosD9 :: Pos x => Pos (x :* D9)

subLastDec :: forall x d. Nat (x :* d) => Nat x => x :* d -> Int
subLastDec = div10Dec >>> toInt >>> (10 * _)

div10Dec :: forall x d. Nat (x :* d) => x :* d -> x
div10Dec _ = undefined

reifyInt :: forall r. Int -> (forall n. Nat n => n -> r) -> r
reifyInt i f
 | i < 0     = unsafeCrashWith "reifyInt: integral < 0"
 | i == 0    = f (undefined :: D0)
 | otherwise = reifyIntP i f

reifyIntP :: forall r. Int -> (forall n. Pos n => n -> r) -> r
reifyIntP i f
  | i < 1 = unsafeCrashWith "reifyIntP: integral < 1"
  | i == 1 = f (undefined :: D1)
  | i == 2 = f (undefined :: D2)
  | i == 3 = f (undefined :: D3)
  | i == 4 = f (undefined :: D4)
  | i == 5 = f (undefined :: D5)
  | i == 6 = f (undefined :: D6)
  | i == 7 = f (undefined :: D7)
  | i == 8 = f (undefined :: D8)
  | i == 9 = f (undefined :: D9)
  | otherwise =
    let d = div i 10
        m = mod i 10
    in unsafePartial $ case m of
      0 -> reifyIntP d f0
      1 -> reifyIntP d f1
      2 -> reifyIntP d f2
      3 -> reifyIntP d f3
      4 -> reifyIntP d f4
      5 -> reifyIntP d f5
      6 -> reifyIntP d f6
      7 -> reifyIntP d f7
      8 -> reifyIntP d f8
      9 -> reifyIntP d f9
    where f0 :: forall e. Pos e => e -> r
          f0 _ = f (undefined :: e :* D0)
          f1 :: forall e. Pos e => e -> r
          f1 _ = f (undefined :: e :* D1)
          f2 :: forall e. Pos e => e -> r
          f2 _ = f (undefined :: e :* D2)
          f3 :: forall e. Pos e => e -> r
          f3 _ = f (undefined :: e :* D3)
          f4 :: forall e. Pos e => e -> r
          f4 _ = f (undefined :: e :* D4)
          f5 :: forall e. Pos e => e -> r
          f5 _ = f (undefined :: e :* D5)
          f6 :: forall e. Pos e => e -> r
          f6 _ = f (undefined :: e :* D6)
          f7 :: forall e. Pos e => e -> r
          f7 _ = f (undefined :: e :* D7)
          f8 :: forall e. Pos e => e -> r
          f8 _ = f (undefined :: e :* D8)
          f9 :: forall e. Pos e => e -> r
          f9 _ = f (undefined :: e :* D9)
