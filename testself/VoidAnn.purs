module TestSelf.VoidAnn where

import Prelude
import CoreFn.Ann (Ann)
import CoreFn.Annotation (class Annotation, annFromJSON)

data VoidAnn
  = VoidAnn

derive instance eqVoidAnn :: Eq VoidAnn

instance showVoidAnn :: Show VoidAnn where
  show VoidAnn = "VoidAnn"

instance annotationVoidAnn :: Annotation VoidAnn where
  annFromJSON modulePath = map annToVoid <<< annFromJSON modulePath
    where
    annToVoid :: Ann -> VoidAnn
    annToVoid _ = VoidAnn
