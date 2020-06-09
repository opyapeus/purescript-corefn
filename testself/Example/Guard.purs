module Testself.Example.Guard
  ( or
  ) where

import Prelude

or :: Boolean -> Boolean -> Boolean
or x y
  | x = true
  | y = true
  | otherwise = false
