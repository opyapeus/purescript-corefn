module Testself.CoreFn.Case
  ( caseTest
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
import Test.Unit (TestSuite, test)
import TestSelf.Main (moduleTest)
import TestSelf.VoidAnn (VoidAnn(..))

caseTest :: TestSuite
caseTest =
  test "case"
    $ moduleTest
        { modulePath: "./output/Testself.Example.Case/corefn.json"
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
          , moduleName: (ModuleName [ (ProperName "Testself"), (ProperName "Example"), (ProperName "Case") ])
          , modulePath: (FilePath "testself/Example/Case.purs")
          , moduleImports: [ (ModuleImport { ann: VoidAnn, moduleName: (ModuleName [ (ProperName "Prim") ]) }) ]
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
                                , ( Var VoidAnn
                                      (Qualified Nothing (Ident "y"))
                                  )
                                ]
                                [ ( CaseAlternative
                                      { caseAlternativeBinders:
                                          [ ( LiteralBinder VoidAnn
                                                (BooleanLiteral true)
                                            )
                                          , (NullBinder VoidAnn)
                                          ]
                                      , caseAlternativeResult: (Right (Literal VoidAnn (BooleanLiteral true)))
                                      }
                                  )
                                , ( CaseAlternative
                                      { caseAlternativeBinders:
                                          [ (NullBinder VoidAnn)
                                          , ( LiteralBinder VoidAnn
                                                (BooleanLiteral true)
                                            )
                                          ]
                                      , caseAlternativeResult:
                                          ( Right
                                              ( Literal VoidAnn
                                                  (BooleanLiteral true)
                                              )
                                          )
                                      }
                                  )
                                , ( CaseAlternative
                                      { caseAlternativeBinders:
                                          [ ( LiteralBinder VoidAnn
                                                (BooleanLiteral false)
                                            )
                                          , ( LiteralBinder VoidAnn
                                                (BooleanLiteral false)
                                            )
                                          ]
                                      , caseAlternativeResult:
                                          ( Right
                                              ( Literal VoidAnn
                                                  (BooleanLiteral false)
                                              )
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
