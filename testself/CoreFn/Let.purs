module Testself.CoreFn.Let
  ( letTest
  ) where

import Prelude
import CoreFn.Expr (Bind(..), Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Literal (Literal(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, test)
import TestSelf.Main (moduleTest)
import TestSelf.VoidAnn (VoidAnn(..))

letTest :: TestSuite
letTest =
  test "let"
    $ moduleTest
        { modulePath: "./output/Testself.Example.Let/corefn.json"
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
          , moduleName: (ModuleName [ (ProperName "Testself"), (ProperName "Example"), (ProperName "Let") ])
          , modulePath: (FilePath "testself/Example/Let.purs")
          , moduleImports:
              [ (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Data"), (ProperName "Semiring") ]) })
              , (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prelude") ]) })
              , (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prim") ]) })
              ]
          , moduleExports: [ (Ident "f") ]
          , moduleForeign: []
          , moduleDecls:
              [ ( NonRec VoidAnn (Ident "f")
                    ( Let VoidAnn
                        [ (NonRec VoidAnn (Ident "y") (Literal VoidAnn (NumericLiteral (Left 2))))
                        , (NonRec VoidAnn (Ident "x") (Literal VoidAnn (NumericLiteral (Left 1))))
                        ]
                        ( App VoidAnn
                            ( App VoidAnn
                                ( App VoidAnn
                                    ( Var VoidAnn
                                        ( Qualified
                                            ( Just
                                                ( ModuleName
                                                    [ (ProperName "Data")
                                                    , (ProperName "Semiring")
                                                    ]
                                                )
                                            )
                                            (Ident "add")
                                        )
                                    )
                                    ( Var VoidAnn
                                        ( Qualified
                                            ( Just
                                                ( ModuleName
                                                    [ (ProperName "Data")
                                                    , (ProperName "Semiring")
                                                    ]
                                                )
                                            )
                                            (Ident "semiringInt")
                                        )
                                    )
                                )
                                (Var VoidAnn (Qualified Nothing (Ident "x")))
                            )
                            (Var VoidAnn (Qualified Nothing (Ident "y")))
                        )
                    )
                )
              ]
          }
      )
  , version: (Version "0.13.6")
  }
