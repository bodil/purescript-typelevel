module Type.Data.Int where

import Prelude
import Type.Data.Nat

import Data.Symbol (SProxy(..))
import Prim.Symbol (class Cons, class Append)
import Type.Data.Boolean (class If)
import Type.Prelude (kind Boolean, True, False, LT, GT, EQ, kind Ordering)
import Unsafe.Coerce (unsafeCoerce)

data IProxy (sym :: Symbol) = IProxy

class IsInt (a :: Symbol) where
  reflectInt :: IProxy a -> Int

class IsSign' (a :: Symbol) (isSign :: Boolean) | a -> isSign, isSign -> a where
  reflectSign :: SProxy a -> Int

instance negSign' :: IsSign' "-" True where
  reflectSign _ = -1
else
instance negElse :: IsSign' a False where
  reflectSign _ = 1

instance posIsInt :: (Cons head tail sym, IsSign' head isHeadSign, If isHeadSign (SProxy tail) (SProxy sym) (SProxy nat), IsNat nat nn) => IsInt sym where
  reflectInt _ = reflectSign (SProxy :: SProxy head) * reflectNat (NProxy :: NProxy nat)

class IntExtract (int :: Symbol) signed | int -> signed

instance intExtract ∷ (Cons head tail int, IsSign' head isHeadSign, If isHeadSign (Neg tail) (Pos int) signed) => IntExtract int signed

class CombineInt signed (int :: Symbol)  | signed -> int

instance posCombine :: CombineInt (Pos n) n
instance negCombine :: Append "-" n n' => CombineInt (Neg n) n'

class IntSignedConvert (int :: Symbol) signed | int -> signed, signed -> int

instance intSignedConvertFromComp :: (IntExtract int signed, CombineInt signed int) => IntSignedConvert int signed

intSignedConvert :: ∀x y. IntSignedConvert x y => IProxy x -> y
intSignedConvert = unsafeCoerce

data Pos (sym :: Symbol)
data Neg (sym :: Symbol)

class (IsInt a, IsInt b) <= Inverse (a :: Symbol) (b :: Symbol) | a -> b, b -> a

instance inverseFromExtracted :: (IsInt a, IsInt b, IntSignedConvert a a', InverseExtracted a' b', IntSignedConvert b b') => Inverse a b

class InverseExtracted a b | a -> b, b -> a

-- inverse 0 -> 0
instance inverse0 :: InverseExtracted (Pos "0") (Pos "0")
else
-- inverse n -> -n
instance inversePos :: InverseExtracted (Pos sym) (Neg sym)
else 
-- inverse -n => n
instance inverseNeg :: InverseExtracted (Neg sym) (Pos sym)

inverse :: ∀x y. Inverse x y => IProxy x -> IProxy y
inverse _ = IProxy

-- sum

class SumExtracted x y z | x y -> z

instance sumPosPos :: Sum a b c => SumExtracted (Pos a) (Pos b) (Pos c)
instance sumNegNeg :: Sum a b c => SumExtracted (Neg a) (Neg b) (Neg c)
instance sumNegPos :: SumExtracted (Pos b) (Neg a) c => SumExtracted (Neg a) (Pos b) c
instance sumPosNeg :: (OrderNat a b ord, SumP a b ord c) => SumExtracted (Pos a) (Neg b) c

class SumP (a :: Symbol) (b :: Symbol) (ord :: Ordering) c | a b ord -> c

instance sumLt :: Sub b a c => SumP a b LT (Neg c)
instance sumGt ∷ Sub a b c => SumP a b GT (Pos c) 
instance sumEq ∷ Sub a b c => SumP a b EQ (Pos c) 

class SumInt (a :: Symbol) (b :: Symbol) (c :: Symbol)

instance sumIntConvert :: (SumExtracted a' b' c', IntSignedConvert a a', IntSignedConvert b b', IntSignedConvert c c') => SumInt a b c

sumInt :: ∀a b c. SumInt a b c => IProxy a -> IProxy b -> IProxy c
sumInt _ _ = IProxy


class IsZeroInt (a :: Symbol) (isZero :: Boolean) | a -> isZero

instance zeroPos :: IsZeroInt "0" True
else
instance zeroNeg ∷ IsZeroInt "-0" True
else
instance zeroElse :: IsZeroInt others False