{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Param where

import qualified Web.Scotty as Scotty
import qualified Control.Exception as E
import Data.String (IsString)
import qualified Eventlog.Args as EA
import Text.Read (readMaybe)
import Model

{- Generic builder for param reader -}
readParam :: t1 -> (String -> t2) -> t2 -> (t1 -> Scotty.ActionM String) -> Scotty.ActionM t2
readParam name f def getParam = do
  result <- (getParam name >>= return . f) `Scotty.catch` (\ (_ :: E.SomeException) -> return def)
  return result

{- Aliases for functions that parse parameters from the web 
 - is intended to be either Scotty.queryParam or Scotty.formParam -}
type ParamGet t r = (t -> Scotty.ActionM String) -> Scotty.ActionM r
selectedParam :: Data.String.IsString t => ParamGet t [Int]
selectedParam = readParam "selected" parsePath [0]
togglePathParam :: Data.String.IsString t => ParamGet t [Int]
togglePathParam = readParam "toggle" parsePath []
profileLevelParam :: Data.String.IsString t => ParamGet t ProfileLevel
profileLevelParam = readParam "profileLevel" parseProfileLevel OneLevel 
filterTypeParam :: Data.String.IsString t => ParamGet t String
filterTypeParam = readParam "filterType" id ""
patternParam :: Data.String.IsString t => ParamGet t String
patternParam = readParam "pattern" id ""
invertParam :: Data.String.IsString t => ParamGet t Bool
invertParam = readParam "invert" read False
indexParam :: Data.String.IsString t => ParamGet t Int
indexParam = readParam "index" read (-1)
searchLimitParam :: Data.String.IsString t => ParamGet t (Maybe Int)
searchLimitParam = readParam "index" readMaybe Nothing
eLogOutParam :: Data.String.IsString t => ParamGet t Bool
eLogOutParam = readParam "isJson" (maybe False id . readMaybe) False
filterChangedParam :: Data.String.IsString t => ParamGet t Bool
filterChangedParam = readParam "filterChanged" (maybe False id . readMaybe) False
fastModeParam :: Data.String.IsString t => ParamGet t Bool
fastModeParam = readParam "fastMode" (maybe False id . readMaybe) False
forceParam :: Data.String.IsString t => ParamGet t [Int]
forceParam = readParam "force" parsePath [0]
eLogSortParam :: Data.String.IsString t => ParamGet t EA.Sort
eLogSortParam = readParam "eventlog-sort" parseSort EA.Size
eLogRevParam :: Data.String.IsString t => ParamGet t Bool
eLogRevParam = readParam "eLogRev" (== "on") False
