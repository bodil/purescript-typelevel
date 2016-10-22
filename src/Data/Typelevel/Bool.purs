module Data.Typelevel.Bool where

import Data.Typelevel.Undefined (undefined)
import Prelude (class Show)

data True

instance showTrue :: Show True where
  show _ = "True"

trueT :: True
trueT = undefined

data False

instance showFalse :: Show False where
  show _ = "False"

falseT :: False
falseT = undefined

class BoolI b where
  toBool :: b -> Boolean

class BoolI b <= Bool b
instance boolIBool :: BoolI b => Bool b

instance boolITrue :: BoolI True where
  toBool _ = true

instance boolIFalse :: BoolI False where
  toBool _ = false

reifyBool :: forall r. Boolean -> (forall b. Bool b => b -> r) -> r
reifyBool true f = f trueT
reifyBool false f = f falseT

class (BoolI b1, BoolI b2) <= Not b1 b2 | b1 -> b2, b2 -> b1
instance notFalse :: Not False True
instance notTrue :: Not True False

not :: forall b1 b2. Not b1 b2 => b1 -> b2
not = undefined

class (BoolI b1, BoolI b2, BoolI b3) <= And b1 b2 b3 | b1 b2 -> b3
instance andFalseFalse :: And False False False
instance andFalseTrue :: And False True False
instance andTrueFalse :: And True False False
instance andTrueTrue :: And True True True

and :: forall b1 b2 b3. And b1 b2 b3 => b1 -> b2 -> b3
and = undefined
infixr 3 and as &&

class (BoolI b1, BoolI b2, BoolI b3) <= Or b1 b2 b3 | b1 b2 -> b3
instance orFalseFalse :: Or False False False
instance orFalseTrue :: Or False True True
instance orTrueFalse :: Or True False True
instance orTrueTrue :: Or True True True

or :: forall b1 b2 b3. Or b1 b2 b3 => b1 -> b2 -> b3
or = undefined
infixr 2 or as ||

class (BoolI b1, BoolI b2, BoolI b3) <= Xor b1 b2 b3 | b1 b2 -> b3
instance xorFalseFalse :: Xor False False False
instance xorFalseTrue :: Xor False True True
instance xorTrueFalse :: Xor True False True
instance xorTrueTrue :: Xor True True False

xor :: forall b1 b2 b3. Xor b1 b2 b3 => b1 -> b2 -> b3
xor = undefined

class (BoolI b1, BoolI b2, BoolI b3) <= Imp b1 b2 b3 | b1 b2 -> b3
instance impFalseFalse :: Imp False False True
instance impFalseTrue :: Imp False True True
instance impTrueFalse :: Imp True False False
instance impTrueTrue :: Imp True True True

imp :: forall b1 b2 b3. Imp b1 b2 b3 => b1 -> b2 -> b3
imp = undefined

class (BoolI b1, BoolI b2, BoolI b3) <= Eq b1 b2 b3 | b1 b2 -> b3
instance eqFalseFalse :: Eq False False True
instance eqFalseTrue :: Eq False True False
instance eqTrueFalse :: Eq True False False
instance eqTrueTrue :: Eq True True True

eq :: forall b1 b2 b3. Eq b1 b2 b3 => b1 -> b2 -> b3
eq = undefined
