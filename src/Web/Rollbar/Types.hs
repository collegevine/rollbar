{-# LANGUAGE TemplateHaskell #-}

module Web.Rollbar.Types
    ( APIToken(APIToken)
    , Environment(Environment)
    , CodeVersion(CodeVersion)
    , Host(Host)
    , RollbarCfg(..)
    , HasRollbarCfg(..)
    , Event(..)
    , EventLevel(..)
    , ToRollbarEvent(..)
    , eventContext
    , eventData
    , eventLevel
    , eventMessage
    , eventTitle
    , eventUUID
    ) where

import Control.Lens.TH (makeClassy, makeLenses)
import Data.Aeson (ToJSON(..), Value)
import Data.Text (Text)

newtype APIToken = APIToken
    { unAPIToken :: Text
    }

instance ToJSON APIToken where
    toJSON = toJSON . unAPIToken

newtype Environment = Environment
    { unEnvironment :: Text
    }

instance ToJSON Environment where
    toJSON = toJSON . unEnvironment

newtype CodeVersion = CodeVersion
    { unCodeVersion :: Text
    }

instance ToJSON CodeVersion where
    toJSON = toJSON . unCodeVersion

newtype Host = Host
    { unHost :: Text
    }

instance ToJSON Host where
    toJSON = toJSON . unHost

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
