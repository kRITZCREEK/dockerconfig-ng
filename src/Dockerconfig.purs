module Dockerconfig (getConfig) where

import Prelude
import Data.List as List
import Data.StrMap as StrMap
import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Argonaut (Json, decodeJson, foldJsonNumber, foldJsonObject, fromObject, jsonParser)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.List (List(..), mapMaybe, (:))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.String (toUpper)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Node.Process (PROCESS, lookupEnv)

type Property = { path :: Path, value :: Json }
type Path = List String

getConfig :: EffFn1 (err :: EXCEPTION, process :: PROCESS, console :: CONSOLE) Json Json
getConfig = mkEffFn1 getConfig'

getConfig' :: forall e. Json -> Eff (err :: EXCEPTION, process :: PROCESS, console :: CONSOLE | e) Json
getConfig' input =
  let
    properties = getProperties input
    updates = for properties \ {path, value} -> do
      envVar <- lookupEnv (pathToEnvVar path)
      case envVar of
        Just new -> do
          let msg = fold
                [ "|"
                , pathToEnvVar path
                , "| -> variable ["
                , List.intercalate "." path
                , "] set, overwriting ["
                , unsafeStringify value
                , "] with [new content..(secret)]"
                ]
          logPurple msg
          setPathObj path <$> either throw pure (jsonParser new)
        Nothing ->
          pure id
  in
   do validateConfigVersion input
      updater <- updates
      pure (List.foldl (<<<) id updater input)

logPurple :: forall e. String -> Eff (console :: CONSOLE | e) Unit
logPurple = withGraphics log (foreground Magenta)

getKeyCI :: forall a. StrMap a -> String -> Maybe String
getKeyCI obj key =
  StrMap.keys obj # List.find \key' -> toUpper key' == toUpper key

pathToEnvVar :: Path -> String
pathToEnvVar = map toUpper >>> List.intercalate "_" >>> ("NODE_CONFIG_" <> _)

lookupPathObj :: Path -> Json -> Maybe Json
lookupPathObj Nil json = Just json
lookupPathObj (next:following) json = foldJsonObject Nothing nextLevel json
  where
    nextLevel obj =
      case getKeyCI obj next of
        Nothing -> Nothing
        Just key -> lookupPathObj following =<< StrMap.lookup key obj

setPathObj :: Path -> Json -> Json -> Json
setPathObj Nil new _ = new
setPathObj (next:following) new json = foldJsonObject json nextLevel json
  where
    nextLevel obj =
      fromObject case getKeyCI obj next of
        Nothing -> obj
        Just key -> StrMap.update (Just <<< setPathObj following new) key obj

getProperties :: Json -> List Property
getProperties json =
  mapMaybe
    (\path -> {path, value: _} <$> lookupPathObj path json)
    (getPaths json)

getPaths :: Json -> List Path
getPaths = foldJsonObject Nil objectToPaths
  where
    objectToPaths obj =
      flip List.concatMap (StrMap.toUnfoldable obj) \(Tuple key value) ->
        case getPaths value of
          Nil -> pure (List.singleton key)
          paths -> Cons key <$> paths

validateConfigVersion :: forall e. Json -> Eff (err :: EXCEPTION, process :: PROCESS | e) Unit
validateConfigVersion json = do
  envConfigVersion <- lookupEnv "NODE_CONFIG_CONFIGVERSION"
  let configVersion = foldJsonObject Nothing (StrMap.lookup "configVersion") json
  case envConfigVersion, configVersion of
   Nothing, Nothing -> pure unit
   Just _, Nothing ->
        throw "config has no version, but NODE_CONFIG_CONFIGVERSION given: please update your image."
   Nothing, Just _ ->
        throw "no NODE_CONFIG_CONFIGVERSION given, but configVersion set in configuration: please update your deployment."
   Just envString, Just cfgString -> do
     cfgV <- foldJsonNumber (throw "configVersion must be a number. Using timestamps as configVersion is strongly suggested.") pure cfgString
     envV <- either
        (\err -> throw ("NODE_CONFIG_CONFIGVERSION must be a number. Using timestamps is strongly suggested. ParseError: " <> err))
        pure
        (decodeJson =<< jsonParser envString)
     when (cfgV < envV)
        (throw "NODE_CONFIG_CONFIGVERSION newer than configVersion: please update your image.")
     when (envV < cfgV)
        (throw "NODE_CONFIG_CONFIGVERSION outdated: please update your deployment.")