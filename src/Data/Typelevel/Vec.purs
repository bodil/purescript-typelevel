module Data.Typelevel.Vec where

import Prelude
import Data.Array as Array
import Data.Foldable (foldl, foldr, foldMap, class Foldable)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Num (class Min, class Sub, class LtEq, class Pred, class Lt)
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (D1, D0)
import Data.Typelevel.Num.Sets (toInt, class Pos, class Nat)
import Data.Typelevel.Undefined (undefined)
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)

newtype Vec s a = Vec (Array a)

empty :: forall a. Vec D0 a
empty = Vec []

cons :: forall s s' a. (Nat s, Pos s', Succ s s') => a -> Vec s a -> Vec s' a
cons x (Vec xs) = Vec $ Array.cons x xs
infixr 5 cons as +>

snoc :: forall s s' a. (Nat s, Pos s', Succ s s') => a -> Vec s a -> Vec s' a
snoc x (Vec xs) = Vec $ Array.snoc xs x

uncons :: forall s1 s2 a. (Pos s1, Pred s1 s2) => Vec s1 a -> { head :: a, tail :: Vec s2 a }
uncons (Vec v) = case unsafePartial $ fromJust $ Array.uncons v of
  { head: h, tail: t } -> { head: h, tail: Vec t }

singleton :: forall a. a -> Vec D1 a
singleton x = x +> empty

replicate :: forall s a. (Nat s) => s -> a -> Vec s a
replicate s a = Vec $ Array.replicate (toInt s) a

length :: forall s a. Nat s => Vec s a -> Int
length _ = toInt (undefined :: s)

lengthT :: forall s a. Nat s => Vec s a -> s
lengthT _ = undefined

toArray :: forall s a. Nat s => Vec s a -> Array a
toArray (Vec xs) = xs

toUnfoldable :: forall f s a. (Unfoldable f, Nat s) => Vec s a -> f a
toUnfoldable (Vec v) = Array.toUnfoldable v

index :: forall i s a. (Nat i, Nat s, Lt i s) => Vec s a -> i -> a
index (Vec xs) i = unsafePartial $ Array.unsafeIndex xs $ toInt i
infixl 8 index as !!

concat :: forall s1 s2 s3 a. (Nat s1, Nat s2, Add s1 s2 s3) => Vec s1 a -> Vec s2 a -> Vec s3 a
concat (Vec xs1) (Vec xs2) = Vec $ Array.concat [xs1, xs2]

updateAt :: forall i s a. (Nat i, Nat s, Lt i s) => i -> a -> Vec s a -> Vec s a
updateAt i v (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.updateAt (toInt i) v xs

modifyAt :: forall i s a. (Nat i, Nat s, Lt i s) => i -> (a -> a) -> Vec s a -> Vec s a
modifyAt i f (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.modifyAt (toInt i) f xs

insertAt :: forall i s1 s2 a. (Nat i, Nat s1, Nat s2, Lt i s1, Succ s1 s2) => i -> a -> Vec s1 a -> Vec s2 a
insertAt i a (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.insertAt (toInt i) a xs

deleteAt :: forall i s1 s2 a. (Nat i, Nat s1, Nat s2, Lt i s1, Pred s1 s2) => i -> Vec s1 a -> Vec s2 a
deleteAt i (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.deleteAt (toInt i) xs

head :: forall s a. (Pos s) => Vec s a -> a
head (Vec xs) = unsafePartial $ fromJust $ Array.head xs

last :: forall s a. (Pos s) => Vec s a -> a
last (Vec xs) = unsafePartial $ fromJust $ Array.last xs

tail :: forall s1 s2 a. (Pos s1, Nat s2, Pred s1 s2) => Vec s1 a -> Vec s2 a
tail (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.tail xs

init :: forall s1 s2 a. (Pos s1, Nat s2, Pred s1 s2) => Vec s1 a -> Vec s2 a
init (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.init xs

insert :: forall s1 s2 a. (Nat s1, Succ s1 s2, Ord a) => a -> Vec s1 a -> Vec s2 a
insert a (Vec v) = Vec $ Array.insert a v

insertBy :: forall s1 s2 a. (Nat s1, Succ s1 s2) => (a -> a -> Ordering) -> a -> Vec s1 a -> Vec s2 a
insertBy f a (Vec v) = Vec $ Array.insertBy f a v

slice :: forall i1 i2 s1 s2 a. (Nat i1, Nat i2, Nat s1, LtEq i1 s1, LtEq i2 s1, LtEq i1 i2, Sub i2 i1 s2) => i1 -> i2 -> Vec s1 a -> Vec s2 a
slice i1 i2 (Vec xs) = Vec $ Array.slice (toInt i1) (toInt i2) xs

take :: forall c s a. (Nat c, Nat s, LtEq c s) => c -> Vec s a -> Vec c a
take c (Vec xs) = Vec $ Array.take (toInt c) xs

drop :: forall c s1 s2 a. (Nat c, Nat s1, LtEq c s1, Sub s1 c s2) => c -> Vec s1 a -> Vec s2 a
drop c (Vec xs) = Vec $ Array.drop (toInt c) xs

zip :: forall s1 s2 s3 a b. (Nat s1, Nat s2, Min s1 s2 s3) => Vec s1 a -> Vec s2 b -> Vec s3 (Tuple a b)
zip (Vec v1) (Vec v2) = Vec $ Array.zip v1 v2

zipWith :: forall s1 s2 s3 a b c. (Nat s1, Nat s2, Min s1 s2 s3) => (a -> b -> c) -> Vec s1 a -> Vec s2 b -> Vec s3 c
zipWith f (Vec v1) (Vec v2) = Vec $ Array.zipWith f v1 v2

unzip :: forall s a b. (Nat s) => Vec s (Tuple a b) -> Tuple (Vec s a) (Vec s b)
unzip (Vec v) = case Array.unzip v of
  (Tuple v1 v2) -> Tuple (Vec v1) (Vec v2)

delete :: forall s1 s2 a. (Pos s1, Pred s1 s2, Eq a) => a -> Vec s1 a -> Vec s2 a
delete a (Vec v) = Vec $ Array.delete a v

sort :: forall s a. (Nat s, Ord a) => Vec s a -> Vec s a
sort (Vec v) = Vec $ Array.sort v

sortBy :: forall s a. (Nat s) => (a -> a -> Ordering) -> Vec s a -> Vec s a
sortBy f (Vec v) = Vec $ Array.sortBy f v

reverse :: forall s a. (Nat s) => Vec s a -> Vec s a
reverse (Vec v) = Vec $ Array.reverse v

instance functorVec :: (Nat s) => Functor (Vec s) where
  map f (Vec xs) = Vec $ map f xs

instance applyVec :: (Nat s) => Apply (Vec s) where
  apply (Vec a) (Vec b) = Vec $ apply a b

instance applicativeVec :: Applicative (Vec D1) where
  pure a = singleton a

instance foldableVec :: (Nat s) => Foldable (Vec s) where
  foldMap f (Vec xs) = foldMap f xs
  foldr f i (Vec xs) = foldr f i xs
  foldl f i (Vec xs) = foldl f i xs

instance eqVec :: (Nat s, Eq a) => Eq (Vec s a) where
  eq (Vec v1) (Vec v2) = v1 == v2

instance showVec :: (Nat s, Show a) => Show (Vec s a) where
  show (Vec v) = show v
