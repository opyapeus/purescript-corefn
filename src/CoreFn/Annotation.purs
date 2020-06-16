module CoreFn.Annotation where

import Prelude

import CoreFn.Ann (Ann(..), SourcePos(..), SourceSpan(..))
import CoreFn.Common (identFromJSON, object)
import CoreFn.Meta (ConstructorType(..), Meta(..))
import CoreFn.Module (FilePath)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign (F, Foreign, ForeignError(..), fail, readArray, readInt, readNull, readString)
import Foreign.Index (index, readProp)

class Annotation a where
  annFromJSON :: FilePath -> Foreign -> F a

instance annotationAnn :: Annotation Ann where
  annFromJSON modulePath = object \json -> do
    sourceSpan <- readProp "sourceSpan" json >>= sourceSpanFromJSON
    meta <- readProp "meta" json >>= readNull >>= traverse metaFromJSON
    pure $ Ann { sourceSpan, comments: [], type: Nothing, meta }
    where
    sourceSpanFromJSON :: Foreign -> F SourceSpan
    sourceSpanFromJSON = object \json -> do
      spanStart <- readProp "start" json >>= sourcePosFromJSON
      spanEnd <- readProp "end" json >>= sourcePosFromJSON
      pure $ SourceSpan { spanName: unwrap modulePath, spanStart, spanEnd }

    sourcePosFromJSON :: Foreign -> F SourcePos
    sourcePosFromJSON json = do
      sourcePosLine <- index json 0 >>= readInt
      sourcePosColumn <- index json 1 >>= readInt
      pure $ SourcePos { sourcePosLine, sourcePosColumn }

    metaFromJSON :: Foreign -> F Meta
    metaFromJSON = object $ \json -> do
      type_ <- readProp "metaType" json >>= readString
      case type_ of
        "IsConstructor" -> isConstructorFromJSON json
        "IsNewtype" -> pure IsNewtype
        "IsTypeClassConstructor" -> pure IsTypeClassConstructor
        "IsForeign" -> pure IsForeign
        "IsWhere" -> pure IsWhere
        _ -> fail $ ForeignError $ "Unknown Meta type :" <> type_
      where
      isConstructorFromJSON :: Foreign -> F Meta
      isConstructorFromJSON json = do
        ct <- readProp "constructorType" json >>= constructorTypeFromJSON
        is <- readProp "identifiers" json >>= readArray >>= traverse identFromJSON
        pure $ IsConstructor ct is

    constructorTypeFromJSON :: Foreign -> F ConstructorType
    constructorTypeFromJSON json = do
      type_ <- readString json
      case type_ of
        "ProductType" -> pure ProductType
        "SumType" -> pure SumType
        _ -> fail $ ForeignError $ "Unknown ConstructorType: " <> type_
