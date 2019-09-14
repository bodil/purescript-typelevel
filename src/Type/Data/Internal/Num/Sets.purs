module Type.Data.Internal.Num.Sets where

import Prelude (unit, (*), (+), (>>>))
import Type.Data.Internal.Num.Reps

import Unsafe.Coerce (unsafeCoerce)

undefined :: ∀a. a
undefined = unsafeCoerce unit


class Nat (n :: Num) where
  toInt :: NumProxy n -> Int

-- | Sugar for `toInt`, providing a more intuitive type
toInt' :: forall n. Nat n => NumProxy n -> Int
toInt' _ = toInt (undefined :: NumProxy n)

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

subLastDec :: forall x d. Nat (x :* d) => Nat x => NumProxy (x :* d) -> Int
subLastDec = div10Dec >>> toInt >>> (10 * _)

div10Dec :: forall x d. Nat (x :* d) => NumProxy (x :* d) -> NumProxy x
div10Dec _ = undefined