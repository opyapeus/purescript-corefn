module Testself.CoreFn.Accessor
  ( accessorTest
  ) where

import Prelude
import CoreFn.Expr (Bind(..), Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, test)
import TestSelf.Main (moduleTest)
import TestSelf.VoidAnn (VoidAnn(..))

accessorTest :: TestSuite
accessorTest =
  test "accessor"
    $ moduleTest
        { modulePath: "./output/Testself.Example.Accessor/corefn.json"
        , expect: expect
        }

expect ::
  { module :: Module VoidAnn
  , version :: Version
  }
expect =
  { module:
      ( Module
          { moduleComments: []
          , moduleName: (ModuleName [ (ProperName "Testself"), (ProperName "Example"), (ProperName "Accessor") ])
          , modulePath: (FilePath "testself/Example/Accessor.purs")
          , moduleImports: [ (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prim") ]) }) ]
          , moduleExports: [ (Ident "recName") ]
          , moduleForeign: []
          , moduleDecls:
              [ ( NonRec VoidAnn (Ident "recName")
                    ( Abs VoidAnn
                        (Ident "r")
                        ( Accessor VoidAnn "name"
                            ( Var VoidAnn
                                (Qualified Nothing (Ident "r"))
                            )
                        )
                    )
                )
              ]
          }
      )
  , version: (Version "0.13.6")
  }
