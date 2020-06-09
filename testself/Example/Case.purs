module Testself.Example.Case
  ( or
  ) where

or :: Boolean -> Boolean -> Boolean
or x y = case x, y of
  true, _ -> true
  _, true -> true
  false, false -> false
