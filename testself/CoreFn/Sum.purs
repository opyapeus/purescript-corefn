module Testself.CoreFn.Sum
  ( sumTest
  ) where

import Prelude
import CoreFn.Expr (Bind(..), Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..))
import Test.Unit (TestSuite, test)
import TestSelf.Main (moduleTest)
import TestSelf.VoidAnn (VoidAnn(..))

sumTest :: TestSuite
sumTest =
  test "sum"
    $ moduleTest
        { modulePath: "./output/Testself.Example.Sum/corefn.json"
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
          , moduleName: (ModuleName [ (ProperName "Testself"), (ProperName "Example"), (ProperName "Sum") ])
          , modulePath: (FilePath "testself/Example/Sum.purs")
          , moduleImports: [ (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prim") ]) }) ]
          , moduleExports: []
          , moduleForeign: []
          , moduleDecls:
              [ ( NonRec VoidAnn
                    (Ident "Apple")
                    ( Constructor VoidAnn
                        (ProperName "Fruit")
                        (ProperName "Apple")
                        []
                    )
                )
              , ( NonRec VoidAnn
                    (Ident "Banana")
                    ( Constructor VoidAnn
                        (ProperName "Fruit")
                        (ProperName "Banana")
                        []
                    )
                )
              ]
          }
      )
  , version: (Version "0.13.6")
  }
