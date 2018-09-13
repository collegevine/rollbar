{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Rollbar.Test where

import Web.Rollbar
    ( AccessToken(..)
    , CodeVersion(..)
    , Environment(..)
    , Event(..)
    , EventLevel(..)
    , HasRollbarCfg(..)
    , Host(..)
    , RollbarCfg(..)
    )
import qualified Web.Rollbar.Internal as I

import Control.Lens.TH (makeLenses)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (ToJSON(..), defaultOptions, genericToEncoding)
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), defConfig, encodePretty')
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, goldenVsString, writeBinaryFile)

newtype Context = Context
    { _ctxRollbarCfg :: RollbarCfg
    }

makeLenses ''Context

instance HasRollbarCfg Context where
    rollbarCfg = ctxRollbarCfg

---
---
---
data SampleData = SampleData
    { sampleFoo :: Bool
    , sampleBar :: Integer
    } deriving (Generic)

instance ToJSON SampleData where
    toEncoding = genericToEncoding defaultOptions

minimalEvent :: Event
minimalEvent =
    Event
    { _eventLevel = Info
    , _eventUUID = Nothing
    , _eventTitle = "title-testing"
    , _eventMessage = "message-testing"
    , _eventData = Nothing
    , _eventContext = Nothing
    }

fullEvent :: Event
fullEvent =
    minimalEvent
    { _eventLevel = Error
    , _eventUUID = Just "deadbeef-dead-beef-dead-deadbeefdead"
    , _eventData = Just (toJSON SampleData {sampleFoo = True, sampleBar = 42})
    , _eventContext = Just "context-testing"
    }

minimalConfig :: RollbarCfg
minimalConfig =
    RollbarCfg
    { _rollbarCfgToken = AccessToken "token-testing"
    , _rollbarCfgEnvironment = Environment "environment-testing"
    , _rollbarCfgHost = Nothing
    , _rollbarCfgCodeVersion = Nothing
    , _rollbarCfgMute = False
    }

fullConfig :: RollbarCfg
fullConfig =
    minimalConfig
    { _rollbarCfgHost = Just (Host "host-testing")
    , _rollbarCfgCodeVersion = Just (CodeVersion "code-version-testing")
    , _rollbarCfgMute = False
    }

---
---
---
test_encodeEvent :: TestTree
test_encodeEvent =
    let events = [("minimal", minimalEvent), ("full", fullEvent)]
        configs = [("minimal", minimalConfig), ("full", fullConfig)]
        tests :: [((String, String), (RollbarCfg, Event))]
        tests =
            [ ( (en <> " event with " <> cn <> " config", "event-" <> en <> "-config-" <> cn)
              , (cv, ev))
            | (en, ev) <- events
            , (cn, cv) <- configs
            ]
    in testGroup "Encoding" $
       map
           (\((name, filename), (config, event)) ->
                (goldenVsString name ("test/golden/" <> filename <> ".json") (test config event)))
           tests
  where
    test :: RollbarCfg -> Event -> IO ByteString
    test cfg evt = do
        let ctx = Context {_ctxRollbarCfg = cfg}
        runReaderT (encodeJSON <$> I.encodeEvent evt) ctx

---
---
---
encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = encodePretty' (defConfig {confIndent = Spaces 2, confTrailingNewline = True})
