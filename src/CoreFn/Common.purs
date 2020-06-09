module CoreFn.Common where

import Prelude

import CoreFn.Ident (Ident(..))
import Foreign (F, Foreign, ForeignError(..), fail, readString, typeOf)

objectType :: String
objectType = "object"

object :: forall a. (Foreign -> F a) -> Foreign -> F a
object _ json
  | typ <- typeOf json, typ /= objectType = fail $ TypeMismatch objectType typ
object f json = f json

identFromJSON :: Foreign -> F Ident
identFromJSON = map Ident <<< readString
