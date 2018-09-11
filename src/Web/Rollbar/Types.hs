{-# LANGUAGE TemplateHaskell #-}

module Web.Rollbar.Types where

import Control.Lens.TH (makeClassy, makeLenses)
import Data.Aeson (Value)
import Data.Text (Text)

newtype APIToken = APIToken
    { unAPIToken :: Text
    }

newtype Environment = Environment
    { unEnvironment :: Text
    }

newtype CodeVersion = CodeVersion
    { unCodeVersion :: Text
    }

newtype Host = Host
    { unHost :: Text
    }

data RollbarCfg = RollbarCfg
    { _rollbarCfgToken :: !APIToken
    , _rollbarCfgEnvironment :: !Environment
    , _rollbarCfgHost :: !(Maybe Host)
    , _rollbarCfgCodeVersion :: !(Maybe CodeVersion)
    , _rollbarCfgMute :: !Bool
    }

data Event = Event
    { _eventLevel :: !EventLevel
    , _eventUUID :: !(Maybe Text)
    , _eventTitle :: !Text
    , _eventMessage :: !Text
    , _eventData :: !(Maybe Value)
    , _eventContext :: !(Maybe Text)
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
