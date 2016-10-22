module Data.Typelevel.Num.Reps
  ( D0, D1, D2, D3, D4, D5, D6, D7, D8, D9
  , d0, d1, d2, d3, d4, d5, d6, d7, d8, d9
  , type (:*), NumCons(NumCons)
  ) where

import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Typelevel.Undefined (undefined)

data D0
data D1
data D2
data D3
data D4
data D5
data D6
data D7
data D8
data D9

data NumCons a b = NumCons a b
infix 6 type NumCons as :*

instance showD0 :: Show D0 where show _ = "0"
instance showD1 :: Show D1 where show _ = "1"
instance showD2 :: Show D2 where show _ = "2"
instance showD3 :: Show D3 where show _ = "3"
instance showD4 :: Show D4 where show _ = "4"
instance showD5 :: Show D5 where show _ = "5"
instance showD6 :: Show D6 where show _ = "6"
instance showD7 :: Show D7 where show _ = "7"
instance showD8 :: Show D8 where show _ = "8"
instance showD9 :: Show D9 where show _ = "9"
instance showDaDb :: (Show a, Show b) => Show (a :* b) where
  show _ = (show (undefined :: a)) <> (show (undefined :: b))

d0 :: D0
d0 = undefined
d1 :: D1
d1 = undefined
d2 :: D2
d2 = undefined
d3 :: D3
d3 = undefined
d4 :: D4
d4 = undefined
d5 :: D5
d5 = undefined
d6 :: D6
d6 = undefined
d7 :: D7
d7 = undefined
d8 :: D8
d8 = undefined
d9 :: D9
d9 = undefined
