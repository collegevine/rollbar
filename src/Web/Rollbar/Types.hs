{-# LANGUAGE TemplateHaskell #-}

module Web.Rollbar.Types where

import Control.Lens.TH
import Data.Aeson (Value)

data RollbarCfg = RollbarCfg {
    _rollbarCfgToken :: String,
    _rollbarCfgEnvironment :: String
}

data Event = Event {
    _eventLevel :: EventLevel,
    _eventUUID :: Maybe String,
    _eventTitle :: String,
    _eventMessage :: String,
    _eventData :: Maybe Value
} deriving Show

data EventLevel = Debug | Info | Warning | Error | Critical deriving Show

makeClassy ''RollbarCfg
makeLenses ''Event
