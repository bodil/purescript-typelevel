module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.Num as Num

main :: Effect Unit
main = runTest do
  Num.tests
