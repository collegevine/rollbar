{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar
    ( module Web.Rollbar.Types
    , rollbar
    ) where

import Web.Rollbar.Types
    ( Event
    , HasRollbarCfg
    , RollbarCfg
    , ToRollbarEvent
    , eventData
    , eventLevel
    , eventMessage
    , eventTitle
    , eventUUID
    , rollbarCfgEnvironment
    , rollbarCfgMute
    , rollbarCfgToken
    , toRollbarEvent
    , unAPIToken
    , unEnvironment
    )

import Control.Lens ((^.), view)
import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson (Value, (.=), object)
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Network.HTTP.Nano
    ( AsHttpError
    , HasHttpCfg
    , HttpMethod(POST)
    , addHeaders
    , buildReq
    , http'
    , mkJSONData
    )

rollbar ::
       ( MonadIO m
       , MonadError e m
       , MonadReader r m
       , AsHttpError e
       , HasHttpCfg r
       , HasRollbarCfg r
       , ToRollbarEvent evt
       )
    => evt
    -> m ()
rollbar evt = do
    isMuted <- view rollbarCfgMute
    unless isMuted $ do
        v <- encodeEvent $ toRollbarEvent evt
        http' . addHeaders [("Content-Type", "application/json")] =<<
            buildReq POST "https://api.rollbar.com/api/1/item/" (mkJSONData v)

-- Documentation: https://docs.rollbar.com/reference
encodeEvent :: (MonadReader r m, HasRollbarCfg r) => Event -> m Value
encodeEvent evt = do
    tok <- view rollbarCfgToken
    env <- view rollbarCfgEnvironment
    return $ object ["access_token" .= unAPIToken tok, "data" .= toData env]
  where
    toData env =
        object $
        [ "environment" .= unEnvironment env
        , "level" .= (toLower <$> show (evt ^. eventLevel))
        , "title" .= (evt ^. eventTitle)
        , "body" .=
          object
              ["message" .= object ["body" .= (evt ^. eventMessage), "data" .= (evt ^. eventData)]]
        , "notifier" .=
          object ["name" .= ("cv-rollbar-haskell" :: Text), "version" .= ("0.2.0" :: Text)]
        ] ++
        maybeToList (("uuid" .=) <$> evt ^. eventUUID)
