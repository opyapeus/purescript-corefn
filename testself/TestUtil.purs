module TestSelf.Main where

import Prelude
import Control.Monad.Except (runExcept)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module, Version)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
-- import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as S
import Node.Path (FilePath)
import Test.Spec.Assertions (fail)
import Test.Unit.Assert as Assert
import TestSelf.VoidAnn (VoidAnn)

type ModuleTestConfig
  = { modulePath :: FilePath
    , expect ::
        { module :: Module VoidAnn
        , version :: Version
        }
    }

moduleTest :: ModuleTestConfig -> Aff Unit
moduleTest config = do
  json <- liftEffect $ S.readTextFile UTF8 config.modulePath
  case runExcept (moduleFromJSON json) of
    Left err -> fail $ show err
    Right mod -> do
      -- log $ show mod
      Assert.equal config.expect mod
