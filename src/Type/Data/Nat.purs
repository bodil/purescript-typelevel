module Type.Data.Nat where

import Prelude

import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Console (log)
import Prim.Symbol (class Cons, class Append)
import Type.Data.Internal.Num.Ops as O
import Type.Data.Internal.Num.Reps as R
import Type.Data.Internal.Num.Sets (class Nat, toInt')
import Type.Data.Ordering (EQ, GT, LT, OProxy(..), kind Ordering)
import Type.Data.Ordering as Ord
import Type.Prelude (False, SProxy(..), True, kind Boolean)
import Unsafe.Coerce (unsafeCoerce)

data NProxy (nat :: Symbol) = NProxy

-- Symbol operations
class MirrorP (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirrorEmpty :: MirrorP "" ""
else
instance mirrorCons :: (Cons head tail sym, MirrorP tailMirror tail, MirrorP tail tailMirror, Append tailMirror head mirror) => MirrorP sym mirror
-- else
-- instance mirrorCons' :: (Cons head tail sym, MirrorP tail tailMirror, Append tailMirror head mirror) => MirrorP mirror sym

class Mirror (a :: Symbol) (mirror :: Symbol) | a -> mirror, mirror -> a

instance mirror :: (MirrorP a b, MirrorP b a) => Mirror a b

mirrorSymbol :: ∀a b. Mirror a b => NProxy a -> NProxy b
mirrorSymbol _ = NProxy

-- Snoc
class Snoc (head :: Symbol) (tail :: Symbol) (symbol :: Symbol) | head tail -> symbol, symbol -> head tail

instance snocMirror :: (Mirror sym mirror, Cons head tailMirror mirror, Mirror tailMirror tail) => Snoc head tail sym

-- SymbolList
foreign import kind SymbolList
foreign import data Cons :: Symbol -> SymbolList -> SymbolList
foreign import data Nil :: SymbolList

data SLProxy (list :: SymbolList) = SLProxy

class SymbolToSymbolList (sym :: Symbol) (list :: SymbolList) | sym -> list, list -> sym

instance emptyList :: SymbolToSymbolList "" Nil
else
instance nonEmptyList :: (Cons head tail sym, SymbolToSymbolList tail tail') => SymbolToSymbolList sym (Cons head tail') 

symbolToList :: ∀sym list. SymbolToSymbolList sym list => SProxy sym -> SLProxy list
symbolToList _ = SLProxy

-- SnocList
class SymbolToSnocList (sym :: Symbol) (list :: SymbolList) | sym -> list, list -> sym

instance emptySnocList :: (Mirror sym sym', SymbolToSymbolList sym' cons) => SymbolToSnocList sym cons

symbolToSnoc :: ∀sym list. SymbolToSnocList sym list => SProxy sym -> SLProxy list
symbolToSnoc _ = SLProxy

-- class IsNat (a :: Symbol) where
--   reflectNat :: NProxy a -> Int

-- instance isNatFromList :: (IsNatList aList, SymbolToSnocList a aList) => IsNat a where
--   reflectNat _ = reflectNatList (undef :: SLProxy aList)

-- class IsNatList (a :: SymbolList) where
--   reflectNatList :: SLProxy a -> Int

-- instance      isNat0 :: IsNatList (Cons "0" Nil) where reflectNatList _ = 0
-- else instance isNat1 :: IsNatList (Cons "1" Nil) where reflectNatList _ = 1
-- else instance isNat2 :: IsNatList (Cons "2" Nil) where reflectNatList _ = 2
-- else instance isNat3 :: IsNatList (Cons "3" Nil) where reflectNatList _ = 3
-- else instance isNat4 :: IsNatList (Cons "4" Nil) where reflectNatList _ = 4
-- else instance isNat5 :: IsNatList (Cons "5" Nil) where reflectNatList _ = 5
-- else instance isNat6 :: IsNatList (Cons "6" Nil) where reflectNatList _ = 6
-- else instance isNat7 :: IsNatList (Cons "7" Nil) where reflectNatList _ = 7
-- else instance isNat8 :: IsNatList (Cons "8" Nil) where reflectNatList _ = 8
-- else instance isNat9 :: IsNatList (Cons "9" Nil) where reflectNatList _ = 9
-- else instance isNatCons :: (IsNat head, IsNatList tail) => IsNatList (Cons head tail) where
--   reflectNatList _ = reflectNat (undef :: NProxy head) * 10 + reflectNatList (undef :: SLProxy tail)



-- Num to Symbol conversion
class IsNat (a :: Symbol) (ty :: R.Num) | a -> ty, ty -> a where
  reflectNat :: NProxy a -> Int

instance isNat0 :: IsNat "0" R.D0 where reflectNat _ = 0
else instance isNat1 :: IsNat "1" R.D1 where reflectNat _ = 1
else instance isNat2 :: IsNat "2" R.D2 where reflectNat _ = 2
else instance isNat3 :: IsNat "3" R.D3 where reflectNat _ = 3
else instance isNat4 :: IsNat "4" R.D4 where reflectNat _ = 4
else instance isNat5 :: IsNat "5" R.D5 where reflectNat _ = 5
else instance isNat6 :: IsNat "6" R.D6 where reflectNat _ = 6
else instance isNat7 :: IsNat "7" R.D7 where reflectNat _ = 7
else instance isNat8 :: IsNat "8" R.D8 where reflectNat _ = 8
else instance isNat9 :: IsNat "9" R.D9 where reflectNat _ = 9
else 
instance isNatCons :: (Snoc head tail sym, Nat (tailD R.:* headD), IsNat head headD, IsNat tail tailD) => IsNat sym (tailD R.:* headD) 
  where reflectNat _ = toInt' (R.NumProxy :: R.NumProxy (tailD R.:* headD))

instance showNat :: (IsNat a a') => Show (NProxy a) where
  show a = show $ reflectNat a

toDigits :: ∀a b. IsNat a b => SProxy a -> R.NumProxy b
toDigits _ = R.NumProxy :: R.NumProxy b


class Sum (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z, z x -> y, z y -> x

instance numToNatSum :: (O.Add a b c, IsNat a' a, IsNat b' b, IsNat c' c) => Sum a' b' c'

addT :: ∀x y z. Sum x y z => NProxy x -> NProxy y -> NProxy z
addT _ _ = NProxy

class DivMod10 (x :: Symbol) (i :: Symbol) (l :: Symbol) | i l -> x, x -> i l

instance numToNatDivMod10 :: (O.DivMod10 x' i' l', IsNat x x', IsNat i i', IsNat l l') => DivMod10 x i l

divMod10 :: ∀ x r q. DivMod10 x q r => NProxy x -> Tuple (NProxy q) (NProxy r)
divMod10 _ = Tuple NProxy NProxy

class OrderNat (x :: Symbol) (y :: Symbol) (order :: Ordering)

instance numToNatOrder :: (O.Trich x' y' order, IsNat x x', IsNat y y') => OrderNat x y order

order :: ∀x y o. OrderNat x y o => NProxy x -> NProxy y -> OProxy o
order _ _ = OProxy

class Sub (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z, z x -> y, z y -> x

instance numToNatSub :: (O.Sub x' y' z', IsNat x x', IsNat y y', IsNat z z') => Sub x y z

subT :: ∀x y z. Sub x y z => NProxy x -> NProxy y -> NProxy z
subT _ _ = NProxy

class Mul (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance numToNatMul :: (O.Mul x' y' z', IsNat x x', IsNat y y', IsNat z z') => Mul x y z

mulT :: ∀x y z. Mul x y z => NProxy x -> NProxy y -> NProxy z
mulT _ _ = NProxy

class Div (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance numToNatDiv :: (O.Div x' y' z', IsNat x x', IsNat y y', IsNat z z') => Div x y z

divT :: ∀x y z. Div x y z => NProxy x -> NProxy y -> NProxy z
divT _ _ = NProxy

class Succ (x :: Symbol) (y :: Symbol) | x -> y, y -> x

instance numTonatSucc :: (O.Succ x' y', IsNat x x', IsNat y y') => Succ x y

succ :: ∀x y. Succ x y => NProxy x -> NProxy y
succ _ = NProxy

class Pred (x :: Symbol) (y :: Symbol) | x -> y, y -> x

instance numTonatPred :: (O.Pred x' y', IsNat x x', IsNat y y') => Pred x y

pred :: ∀x y. Pred x y => NProxy x -> NProxy y
pred _ = NProxy

class Max (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance numToNatMax ∷ (IsNat x x', IsNat y y', IsNat z z', O.Max x' y' z') => Max x y z

max :: ∀x y z. Max x y z => NProxy x -> NProxy y -> NProxy z
max _ _ = NProxy

class Min (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance numToNatMin ∷ (IsNat x x', IsNat y y', IsNat z z', O.Min x' y' z') => Min x y z

min :: ∀x y z. Min x y z => NProxy x -> NProxy y -> NProxy z
min _ _ = NProxy

class Gcd (x :: Symbol) (y :: Symbol) (gcd :: Symbol) | x y -> gcd

instance numToNatGcd :: (IsNat x x', IsNat y y', IsNat gcd gcd', O.GCD x' y' gcd') => Gcd x y gcd

gcd :: ∀x y gcd. Gcd x y gcd => NProxy x -> NProxy y -> NProxy gcd
gcd _ _ = NProxy