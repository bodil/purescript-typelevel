module Data.Typelevel.Bool
  ( True
  , trueT
  , False
  , falseT
  , class Bool
  , class BoolI
  , reifyBool
  , toBool
  , class Not
  , class And
  , class Or
  , class Xor
  , class Imp
  , class Eq
  , not
  , and
  , or
  , xor
  , imp
  , eq
  ) where

import Data.Show (class Show)
import Data.Typelevel.Undefined (undefined)

-- | The type level True value.
data True

instance showTrue :: Show True where
  show _ = "True"

trueT :: True
trueT = undefined

-- | The type level False value.
data False

instance showFalse :: Show False where
  show _ = "False"

falseT :: False
falseT = undefined

class BoolI b where
  toBool :: b -> Boolean

-- | A type level boolean constraint.
-- |
-- | Use `(Bool b) => b` to express that the type `b` must be
-- | either `True` or `False`.
class BoolI b <= Bool b
instance boolIBool :: BoolI b => Bool b

instance boolITrue :: BoolI True where
  toBool _ = true

instance boolIFalse :: BoolI False where
  toBool _ = false

-- | Convert a value level boolean into a type level boolean
-- | through a callback function.
reifyBool :: forall r. Boolean -> (forall b. Bool b => b -> r) -> r
reifyBool true f = f trueT
reifyBool false f = f falseT

-- | Type level logical not.
-- |
-- | `(Not a b) => a -> b` applies the constraint that `a` and `b`
-- | must be logical opposites.
class (BoolI b1, BoolI b2) <= Not b1 b2 | b1 -> b2, b2 -> b1
instance notFalse :: Not False True
instance notTrue :: Not True False

not :: forall b1 b2. Not b1 b2 => b1 -> b2
not = undefined

-- | Type level logical and.
-- |
-- | `(And a b c) => a -> b -> c` applies the constraint that `c` must be
-- | the result of applying a logical and operation to `a` and `b`.
class (BoolI b1, BoolI b2, BoolI b3) <= And b1 b2 b3 | b1 b2 -> b3
instance andFalseFalse :: And False False False
instance andFalseTrue :: And False True False
instance andTrueFalse :: And True False False
instance andTrueTrue :: And True True True

and :: forall b1 b2 b3. And b1 b2 b3 => b1 -> b2 -> b3
and = undefined
infixr 3 and as &&

-- | Type level logical or.
-- |
-- | `(Or a b c) => a -> b -> c` applies the constraint that `c` must be
-- | the result of applying a logical or operation to `a` and `b`.
class (BoolI b1, BoolI b2, BoolI b3) <= Or b1 b2 b3 | b1 b2 -> b3
instance orFalseFalse :: Or False False False
instance orFalseTrue :: Or False True True
instance orTrueFalse :: Or True False True
instance orTrueTrue :: Or True True True

or :: forall b1 b2 b3. Or b1 b2 b3 => b1 -> b2 -> b3
or = undefined
infixr 2 or as ||

-- | Type level logical xor.
-- |
-- | `(Xor a b c) => a -> b -> c` applies the constraint that `c` must be
-- | the result of applying a logical xor operation to `a` and `b`.
class (BoolI b1, BoolI b2, BoolI b3) <= Xor b1 b2 b3 | b1 b2 -> b3
instance xorFalseFalse :: Xor False False False
instance xorFalseTrue :: Xor False True True
instance xorTrueFalse :: Xor True False True
instance xorTrueTrue :: Xor True True False

xor :: forall b1 b2 b3. Xor b1 b2 b3 => b1 -> b2 -> b3
xor = undefined

-- | Type level logical implication.
-- |
-- | `(Imp a b c) => a -> b -> c` applies the constraint that `c` must be
-- | the result of applying a logical implication operation to `a` and `b`.
class (BoolI b1, BoolI b2, BoolI b3) <= Imp b1 b2 b3 | b1 b2 -> b3
instance impFalseFalse :: Imp False False True
instance impFalseTrue :: Imp False True True
instance impTrueFalse :: Imp True False False
instance impTrueTrue :: Imp True True True

imp :: forall b1 b2 b3. Imp b1 b2 b3 => b1 -> b2 -> b3
imp = undefined

-- | Boolean equality check.
-- |
-- | `(Eq a b c) => a -> b -> c` applies the constraint that `c` must be
-- | the result of testing whether `a` and `b` are equal.
class (BoolI b1, BoolI b2, BoolI b3) <= Eq b1 b2 b3 | b1 b2 -> b3
instance eqFalseFalse :: Eq False False True
instance eqFalseTrue :: Eq False True False
instance eqTrueFalse :: Eq True False False
instance eqTrueTrue :: Eq True True True

eq :: forall b1 b2 b3. Eq b1 b2 b3 => b1 -> b2 -> b3
eq = undefined
