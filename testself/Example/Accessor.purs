module Testself.Example.Accessor
  ( recName
  ) where

recName :: { name :: String } -> String
recName r = r.name
