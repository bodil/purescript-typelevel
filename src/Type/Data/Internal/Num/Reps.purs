module Type.Data.Internal.Num.Reps where

foreign import kind Num

foreign import data D0 :: Num
foreign import data D1 :: Num
foreign import data D2 :: Num
foreign import data D3 :: Num
foreign import data D4 :: Num
foreign import data D5 :: Num
foreign import data D6 :: Num
foreign import data D7 :: Num
foreign import data D8 :: Num
foreign import data D9 :: Num

data NumProxy (d :: Num) = NumProxy

foreign import data NumCons :: Num -> Num -> Num
infixl 6 type NumCons as :*