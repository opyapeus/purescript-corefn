module Testself.CoreFn.Foreign
  ( foreignTest
  ) where

import Prelude
import CoreFn.Ident (Ident(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..))
import Test.Unit (TestSuite, test)
import TestSelf.Main (moduleTest)
import TestSelf.VoidAnn (VoidAnn(..))

foreignTest :: TestSuite
foreignTest =
  test "foreign"
    $ moduleTest
        { modulePath: "./output/Testself.Example.Foreign/corefn.json"
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
          , moduleName: (ModuleName [ (ProperName "Testself"), (ProperName "Example"), (ProperName "Foreign") ])
          , modulePath: (FilePath "testself/Example/Foreign.purs")
          , moduleImports: [ (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prim") ]) }) ]
          , moduleExports: [ (Ident "plusOne") ]
          , moduleForeign: [ (Ident "plusOne") ]
          , moduleDecls: []
          }
      )
  , version: (Version "0.13.6")
  }
