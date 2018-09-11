{-# LANGUAGE TemplateHaskell #-}

module Web.Rollbar.Types where

import Control.Lens.TH (makeClassy, makeLenses)
import Data.Aeson (Value)
import Data.Text (Text)

data RollbarCfg = RollbarCfg
    { _rollbarCfgToken :: !String
    , _rollbarCfgEnvironment :: !String
    , _rollbarCfgCodeVersion :: !(Maybe Text)
    , _rollbarCfgHost :: !(Maybe Text)
    , _rollbarCfgMute :: !Bool
    }

data Event = Event
    { _eventLevel :: !EventLevel
    , _eventUUID :: !(Maybe String)
    , _eventTitle :: !String
    , _eventMessage :: !String
    , _eventData :: !(Maybe Value)
    } deriving (Show)

class ToRollbarEvent e where
    toRollbarEvent :: e -> Event

instance ToRollbarEvent Event where
    toRollbarEvent = id

data EventLevel
    = Debug
    | Info
    | Warning
    | Error
    | Critical
    deriving (Show)

makeClassy ''RollbarCfg

makeLenses ''Event
