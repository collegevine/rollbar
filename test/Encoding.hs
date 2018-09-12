{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoding where

import Control.Lens.TH (makeLenses)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), defConfig, encodePretty')
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, goldenVsString, writeBinaryFile)
import Web.Rollbar
    ( APIToken(..)
    , CodeVersion(..)
    , Environment(..)
    , Event(..)
    , EventLevel(..)
    , HasRollbarCfg(..)
    , Host(..)
    , RollbarCfg(..)
    , encodeEvent
    )

data Context = Context
    { _ctxRollbarCfg :: RollbarCfg
    }

makeLenses ''Context

instance HasRollbarCfg Context where
    rollbarCfg = ctxRollbarCfg

---
---
---
sampleEvent :: Event
sampleEvent =
    Event
    { _eventLevel = Warning
    , _eventUUID = Nothing
    , _eventTitle = "Event Title"
    , _eventMessage = "Event Message"
    , _eventData = Nothing
    , _eventContext = Nothing
    }

---
---
---
test_encodeEvent :: TestTree
test_encodeEvent =
    testGroup
        "Encoding"
        [goldenVsString "full event" "test/golden/event-1.json" (encodeEvent' sampleEvent)]
  where
    encodeEvent' e = do
        let ctx = Context {_ctxRollbarCfg = rollbarCfg}
            rollbarCfg =
                RollbarCfg
                { _rollbarCfgToken = APIToken "token-testing"
                , _rollbarCfgEnvironment = Environment "environment-testing"
                , _rollbarCfgHost = Just (Host "host-testing")
                , _rollbarCfgCodeVersion = Just (CodeVersion "code-version-testing")
                , _rollbarCfgMute = False
                }
        runReaderT (encodeJSON <$> encodeEvent e) ctx

---
---
---
encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = encodePretty' (defConfig {confIndent = (Spaces 2), confTrailingNewline = True})
