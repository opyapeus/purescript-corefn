module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Testself.CoreFn.Accessor (accessorTest)
import Testself.CoreFn.Case (caseTest)
import Testself.CoreFn.Foreign (foreignTest)
import Testself.CoreFn.Guard (guardTest)
import Testself.CoreFn.Let (letTest)
import Testself.CoreFn.Sum (sumTest)

main :: Effect Unit
main =
  runTest do
    accessorTest
    caseTest
    foreignTest
    guardTest
    letTest
    sumTest
