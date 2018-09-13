{-# LANGUAGE OverloadedStrings #-}
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
import Data.Aeson (ToJSON(..), Value(String))
import Data.Ord (Ord)
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
    }

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
    deriving (Eq, Ord)

instance ToJSON EventLevel where
    toJSON Debug = String "debug"
    toJSON Info = String "info"
    toJSON Warning = String "warning"
    toJSON Error = String "error"
    toJSON Critical = String "critical"

makeClassy ''RollbarCfg

makeLenses ''Event
