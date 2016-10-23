# purescript-typelevel

Type level natural numbers and booleans.

This package is a direct port of the Haskell [type-level](https://github.com/forsyde/type-level) package by Acosta, Kiselyov, Jeltsch et al. The original BSD 3-clause licence is preserved for this derivative work.

* [API docs on Pursuit](http://pursuit.purescript.org/packages/purescript-typelevel/)

## Type Level Numbers

This package provides a way to represent type level natural numbers and booleans, with a useful set of operations for both.

A natural number is expressed at the type level using uninhabited types with a `D` prefixed to the number: `D0`, `D1`, `D2` etc. You can use the `:*` type operator to combine digits: eg. `D1 :* D3 :* D3 :* D7` makes the number 1337. Aliases are provided for two digit numbers: `D10`, `D11` etc. up to `D99`. Values of each type are also provided, with the lower case prefix `d`, so: `d0`, `d1`, `d2` etc. up to `d99`.

Two type classes are provided acting as sets: `Nat` is the set of natural numbers (zero to infinity), and `Pos` is the set of positive numbers (1 to infinity). `Nat` defines the function `toInt`, which converts type level numbers to the value level. To move integer values to the type level, use the `reifyInt` function (which will throw a runtime exception for values which are not natural numbers).

You can express arithmetic operators as type class constraints on type level numbers. For instance, to express that a type `c` is the sum of types `a` and `b`, you would use the type signature `∀ a b c. (Nat a, Nat b, Add a b c) ⇒ a → b → c`. To express that a type `a` must be less than another type `b`, you would use the type signature `∀ a b. (Nat a, Nat b, Lt a b) ⇒ a → b`.

Most of these constraints are fully relational, so that (even though a `Sub` constraint exists) you could make `c` the result of subtracting `b` from `a` by saying `∀ a b c. (Add c b a) ⇒ a → b → c`.

Note that while most of the arithmetic operations are reasonably performant, division (`DivMod` and friends) is not: division with a three digit divisor takes more than five minutes to type check on my machine.

## LICENCE

Copyright (c) 2016 Bodil Stokke

Copyright (c) 2008 Alfonso Acosta, Oleg Kiselyov, Wolfgang Jeltsch and
 SAM Group at the School of Information and Communication  Technology,
 (Royal Institute of Technology, Stockholm, Sweden)

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
* Neither the name of The ForSyDe Team nor the
  names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS TEAM BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
