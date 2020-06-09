module Testself.CoreFn.Guard
  ( guardTest
  ) where

import Prelude
import CoreFn.Binders (Binder(..))
import CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Literal (Literal(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Unit (TestSuite, test)
import TestSelf.Main (moduleTest)
import TestSelf.VoidAnn (VoidAnn(..))

guardTest :: TestSuite
guardTest =
  test "guard"
    $ moduleTest
        { modulePath: "./output/Testself.Example.Guard/corefn.json"
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
          , moduleName: (ModuleName [ (ProperName "Testself"), (ProperName "Example"), (ProperName "Guard") ])
          , modulePath: (FilePath "testself/Example/Guard.purs")
          , moduleImports: [ (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Data"), (ProperName "Boolean") ]) }), (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prelude") ]) }), (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prim") ]) }) ]
          , moduleExports: [ (Ident "or") ]
          , moduleForeign: []
          , moduleDecls:
              [ ( NonRec VoidAnn
                    (Ident "or")
                    ( Abs VoidAnn
                        (Ident "x")
                        ( Abs VoidAnn
                            (Ident "y")
                            ( Case VoidAnn
                                [ ( Var VoidAnn
                                      (Qualified Nothing (Ident "x"))
                                  )
                                , (Var VoidAnn (Qualified Nothing (Ident "y")))
                                ]
                                [ ( CaseAlternative
                                      { caseAlternativeBinders:
                                          [ ( VarBinder VoidAnn
                                                (Ident "x1")
                                            )
                                          , (VarBinder VoidAnn (Ident "y1"))
                                          ]
                                      , caseAlternativeResult:
                                          ( Left
                                              [ ( Tuple
                                                    ( Var VoidAnn
                                                        (Qualified Nothing (Ident "x1"))
                                                    )
                                                    (Literal VoidAnn (BooleanLiteral true))
                                                )
                                              , ( Tuple
                                                    ( Var VoidAnn
                                                        (Qualified Nothing (Ident "y1"))
                                                    )
                                                    (Literal VoidAnn (BooleanLiteral true))
                                                )
                                              , ( Tuple
                                                    ( Var VoidAnn
                                                        (Qualified (Just (ModuleName [ (ProperName "Data"), (ProperName "Boolean") ])) (Ident "otherwise"))
                                                    )
                                                    ( Literal VoidAnn
                                                        (BooleanLiteral false)
                                                    )
                                                )
                                              ]
                                          )
                                      }
                                  )
                                ]
                            )
                        )
                    )
                )
              ]
          }
      )
  , version: (Version "0.13.6")
  }
