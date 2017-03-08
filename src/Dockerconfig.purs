module Dockerconfig (getConfig) where

import Prelude
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.StrMap as StrMap
import Data.String as String
import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Argonaut (Json, decodeJson, foldJsonNumber, foldJsonObject, fromObject, fromString, jsonParser)
import Data.Either (either)
import Data.Foldable (elem, fold)
import Data.Function (on)
import Data.Function.Eff (EffFn1, mkEffFn1)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Node.Process (PROCESS, getEnv, lookupEnv)

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
          pure (setPathObj path (either (const (fromString new)) id (jsonParser new)))
        Nothing ->
          pure id
  in
   do validateConfigVersion input
      checkRedundantEnv (map _.path properties) =<< getEnv
      checkOverlappingProps (map _.path properties)
      updater <- updates
      pure (List.foldl (<<<) id updater input)

logPurple :: forall e. String -> Eff (console :: CONSOLE | e) Unit
logPurple = withGraphics log (foreground Magenta)

logWarn :: forall e. String -> Eff (console :: CONSOLE | e) Unit
logWarn = withGraphics log (foreground Yellow) <<< ("[WARN - dockerconfig] "<> _)

getKeyCI :: forall a. StrMap a -> String -> Maybe String
getKeyCI obj key =
  StrMap.keys obj # List.find \key' -> String.toUpper key' == String.toUpper key

pathToEnvVar :: Path -> String
pathToEnvVar = map String.toUpper >>> List.intercalate "_" >>> ("NODE_CONFIG_" <> _)

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
  List.mapMaybe
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

-- | Checks whether any property paths overlap when being mapped to environment variables
checkOverlappingProps :: forall e. List Path -> Eff (err :: EXCEPTION | e) Unit
checkOverlappingProps properties =
  unless (List.null duplicates) do
    throw ("Overlapping properties in dockerconfig:\n" <>
           List.intercalate "\n"
             (map
              (NonEmpty.toList
               >>> map (\dup -> List.intercalate "." dup)
               >>> List.intercalate ", ")
              duplicates))
  where
   duplicates =
     properties
     # List.sortBy (compare `on` pathToEnvVar)
     # List.groupBy ((==) `on` pathToEnvVar)
     # List.filter (NonEmpty.length >>> (_ > 1))

-- | Checks whether any Environment variables prefixed with DOCKER_CONFIG did not
-- | match a property
checkRedundantEnv :: forall a e. List Path -> StrMap a -> Eff (console :: CONSOLE | e) Unit
checkRedundantEnv paths env =
  unless (List.null redundantEnvVars)
    (logWarn ("Didn't find any properties to update with these environment variables: " <> List.intercalate ", " redundantEnvVars))
  where
    redundantEnvVars = env
                       # StrMap.keys
                       # List.fromFoldable
                       # List.filter ((_ == Just 0) <<< String.indexOf (String.Pattern "NODE_CONFIG"))
                       # List.filter (not <<< (_ `elem` mappedPaths))
    mappedPaths = map pathToEnvVar paths
