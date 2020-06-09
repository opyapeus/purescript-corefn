module Testself.Example.Let
  ( f
  ) where

import Prelude

f :: Int
f =
  let
    x = 1

    y = 2
  in
    x + y
