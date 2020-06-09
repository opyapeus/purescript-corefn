module CoreFn.FromJSON
  ( moduleFromJSON
  ) where

import Prelude

import Control.Alt ((<|>))
import CoreFn.Ann (Comment(..))
import CoreFn.Annotation (class Annotation, annFromJSON)
import CoreFn.Binders (Binder(..))
import CoreFn.Common (identFromJSON, object)
import CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Literal (Literal(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString)
import Foreign.Index (readIndex, readProp)
import Foreign.JSON (parseJSON)
import Foreign.Keys (keys)

literalFromJSON :: forall a. (Foreign -> F a) -> Foreign -> F (Literal a)
literalFromJSON t = object \json -> do
  type_ <- readProp "literalType" json >>= readString
  case type_ of
    "IntLiteral" ->
      NumericLiteral <<< Left <$> (readProp "value" json >>= readInt)
    "NumberLiteral" ->
      NumericLiteral <<< Right <$> (readProp "value" json >>= readNumber)
    "StringLiteral" ->
      StringLiteral <$> (readProp "value" json >>= readString)
    "CharLiteral" ->
      CharLiteral <$> (readProp "value" json >>= readChar)
    "BooleanLiteral" ->
      BooleanLiteral <$> (readProp "value" json >>= readBoolean)
    "ArrayLiteral" -> parseArrayLiteral json
    "ObjectLiteral" -> parseObjectLiteral json
    _ -> fail $ ForeignError $ "Unknown Literal: " <> type_
  where
    parseArrayLiteral :: Foreign -> F (Literal a)
    parseArrayLiteral json = do
      val <- readProp "value" json >>= readArray
      as <- traverse t val
      pure $ ArrayLiteral as

    parseObjectLiteral :: Foreign -> F (Literal a)
    parseObjectLiteral json = do
      val <- readProp "value" json
      ObjectLiteral <$> recordFromJSON t val

properNameFromJSON :: Foreign -> F ProperName
properNameFromJSON = map ProperName <<< readString

qualifiedFromJSON :: forall a. (String -> a) -> Foreign -> F (Qualified a)
qualifiedFromJSON f = object \json -> do
  mn <- readProp "moduleName" json >>= readNull >>= traverse moduleNameFromJSON
  i <- readProp "identifier" json >>= map f <<< readString
  pure $ Qualified mn i

moduleNameFromJSON :: Foreign -> F ModuleName
moduleNameFromJSON json = map ModuleName $ readArray json
  >>= traverse properNameFromJSON

moduleFromJSON :: forall a. Annotation a => String -> F { version :: Version, module :: Module a }
moduleFromJSON = parseJSON >=> moduleFromJSON'
  where
  moduleFromJSON' :: Foreign -> F { version :: Version, module :: Module a }
  moduleFromJSON' = object \json -> do
    version <- map Version $ readProp "builtWith" json >>= readString

    moduleName <- readProp "moduleName" json >>= moduleNameFromJSON

    modulePath <- map FilePath $ readProp "modulePath" json >>= readString

    moduleImports <- readProp "imports" json
      >>= readArray
      >>= traverse (importFromJSON modulePath)

    moduleExports <- readProp "exports" json
      >>= readArray
      >>= traverse identFromJSON

    moduleDecls <- readProp "decls" json
      >>= readArray
      >>= traverse (bindFromJSON modulePath)

    moduleForeign <- readProp "foreign" json
      >>= readArray
      >>= traverse identFromJSON

    moduleComments <- readProp "comments" json
      >>= readArray
      >>= traverse commentFromJSON

    pure
      { version
      , module: Module
        { moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        }
      }

  importFromJSON
    :: FilePath
    -> Foreign
    -> F (ModuleImport a)
  importFromJSON modulePath = object \json -> do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    moduleName <- readProp "moduleName" json >>= moduleNameFromJSON
    pure $ ModuleImport { ann,  moduleName }

  commentFromJSON :: Foreign -> F Comment
  commentFromJSON json =
    lineCommentFromJSON json
      <|> blockCommentFromJSON json
      <|> invalidComment json
    where
    blockCommentFromJSON :: Foreign -> F Comment
    blockCommentFromJSON =
      readProp "BlockComment" >=> map BlockComment <<< readString

    lineCommentFromJSON :: Foreign -> F Comment
    lineCommentFromJSON =
      readProp "LineComment" >=> map LineComment <<< readString

    invalidComment :: Foreign -> F Comment
    invalidComment = keys >=> Array.head >>> case _ of
      Just type_ -> fail $ ForeignError $ "Unknown Comment type: " <> type_
      Nothing -> fail $ ForeignError "Invalid Comment"

bindFromJSON :: forall a. Annotation a => FilePath -> Foreign -> F (Bind a)
bindFromJSON modulePath = object \json -> do
  type_ <- readProp "bindType" json >>= readString
  case type_ of
    "NonRec" -> (uncurry <<< uncurry) NonRec <$> bindFromJSON' json
    "Rec" ->
      map Rec
        $ readProp "binds" json
        >>= readArray
        >>= traverse (object bindFromJSON')
    _ -> fail $ ForeignError $ "Unknown Bind type: " <> type_
  where
  bindFromJSON' :: Foreign -> F (Tuple (Tuple a Ident) (Expr a))
  bindFromJSON' json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    ident <- readProp "identifier" json >>= identFromJSON
    expr <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Tuple (Tuple ann ident) expr

recordFromJSON
  :: forall a
   . (Foreign -> F a)
  -> Foreign
  -> F (Array (Tuple String a))
recordFromJSON p json = readArray json >>= traverse parsePair
  where
  parsePair :: Foreign -> F (Tuple String a)
  parsePair v = do
    l <- readIndex 0 v >>= readString
    a <- readIndex 1 v >>= p
    pure $ Tuple l a

exprFromJSON :: forall a. Annotation a => FilePath -> Foreign -> F (Expr a)
exprFromJSON modulePath = object \json -> do
  type_ <- readProp "type" json >>= readString
  case type_ of
    "Var" -> varFromJSON json
    "Literal" -> literalExprFromJSON json
    "Constructor" -> constructorFromJSON json
    "Accessor" -> accessorFromJSON json
    "ObjectUpdate" -> objectUpdateFromJSON json
    "Abs" -> absFromJSON json
    "App" -> appFromJSON json
    "Case" -> caseFromJSON json
    "Let" -> letFromJSON json
    _ -> fail $ ForeignError $ "Unknown Expr type: " <> type_
  where
  varFromJSON :: Foreign -> F (Expr a)
  varFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    qi <- readProp "value" json >>= qualifiedFromJSON Ident
    pure $ Var ann qi

  literalExprFromJSON :: Foreign -> F (Expr a)
  literalExprFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    lit <- readProp "value" json >>= literalFromJSON (exprFromJSON modulePath)
    pure $ Literal ann lit

  constructorFromJSON :: Foreign -> F (Expr a)
  constructorFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    tyn <- readProp "typeName" json >>= properNameFromJSON
    con <- readProp "constructorName" json >>= properNameFromJSON
    is  <- readProp "fieldNames" json >>= readArray >>= traverse identFromJSON
    pure $ Constructor ann tyn con is

  accessorFromJSON :: Foreign -> F (Expr a)
  accessorFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    f <- readProp "fieldName" json >>= readString
    e <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Accessor ann f e

  objectUpdateFromJSON :: Foreign -> F (Expr a)
  objectUpdateFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    e <- readProp "expression" json >>= exprFromJSON modulePath
    us <- readProp "updates" json >>= recordFromJSON (exprFromJSON modulePath)
    pure $ ObjectUpdate ann e us

  absFromJSON :: Foreign -> F (Expr a)
  absFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    idn <- readProp "argument" json >>= identFromJSON
    e <- readProp "body" json >>= exprFromJSON modulePath
    pure $ Abs ann idn e

  appFromJSON :: Foreign -> F (Expr a)
  appFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    e <- readProp "abstraction" json >>= exprFromJSON modulePath
    e' <- readProp "argument" json >>= exprFromJSON modulePath
    pure $ App ann e e'

  caseFromJSON :: Foreign -> F (Expr a)
  caseFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    cs <- readProp "caseExpressions" json
      >>= readArray
      >>= traverse (exprFromJSON modulePath)
    cas <- readProp "caseAlternatives" json
      >>= readArray
      >>= traverse (caseAlternativeFromJSON modulePath)
    pure $ Case ann cs cas

  letFromJSON :: Foreign -> F (Expr a)
  letFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    bs <- readProp "binds" json
      >>= readArray
      >>= traverse (bindFromJSON modulePath)
    e <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Let ann bs e

caseAlternativeFromJSON :: forall a. Annotation a => FilePath -> Foreign -> F (CaseAlternative a)
caseAlternativeFromJSON modulePath = object \json -> do
  bs <- readProp "binders" json
    >>= readArray
    >>= traverse (binderFromJSON modulePath)
  isGuarded <- readProp "isGuarded" json >>= readBoolean
  if isGuarded
    then do
      es <- readProp "expressions" json
        >>= readArray
        >>= traverse parseResultWithGuard
      pure $ CaseAlternative
        { caseAlternativeBinders: bs
        , caseAlternativeResult: Left es
        }
    else do
      e <- readProp "expression" json >>= exprFromJSON modulePath
      pure $ CaseAlternative
        { caseAlternativeBinders: bs
        , caseAlternativeResult: Right e
        }
  where
  parseResultWithGuard :: Foreign -> F (Tuple (Expr a) (Expr a))
  parseResultWithGuard = object \json -> do
    g <- readProp "guard" json >>= exprFromJSON modulePath
    e <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Tuple g e

binderFromJSON :: forall a. Annotation a => FilePath -> Foreign -> F (Binder a)
binderFromJSON modulePath = object \json -> do
  type_ <- readProp "binderType" json >>= readString
  case type_ of
    "NullBinder" -> nullBinderFromJSON json
    "VarBinder" -> varBinderFromJSON json
    "LiteralBinder" -> literalBinderFromJSON json
    "ConstructorBinder" -> constructorBinderFromJSON json
    "NamedBinder" -> namedBinderFromJSON json
    _ -> fail $ ForeignError $ "Unknown Binder type: " <> type_
  where
  nullBinderFromJSON :: Foreign -> F (Binder a)
  nullBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    pure $ NullBinder ann

  varBinderFromJSON :: Foreign -> F (Binder a)
  varBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    idn <- readProp "identifier" json >>= identFromJSON
    pure $ VarBinder ann idn

  literalBinderFromJSON :: Foreign -> F (Binder a)
  literalBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    lit <- readProp "literal" json
      >>= literalFromJSON (binderFromJSON modulePath)
    pure $ LiteralBinder ann lit

  constructorBinderFromJSON :: Foreign -> F (Binder a)
  constructorBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    tyn <- readProp "typeName" json >>= qualifiedFromJSON ProperName
    con <- readProp "constructorName" json >>= qualifiedFromJSON ProperName
    bs <- readProp "binders" json
      >>= readArray
      >>= traverse (binderFromJSON modulePath)
    pure $ ConstructorBinder ann tyn con bs

  namedBinderFromJSON :: Foreign -> F (Binder a)
  namedBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    n <- readProp "identifier" json >>= identFromJSON
    b <- readProp "binder" json >>= binderFromJSON modulePath
    pure $ NamedBinder ann n b
