{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar
    ( module Web.Rollbar.Types
    , rollbar
    , encodeEvent
    ) where

import Web.Rollbar.Types

import Control.Lens ((^.), view)
import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson (Value, (.=), object)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
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
    mhost <- view rollbarCfgHost
    mcv <- view rollbarCfgCodeVersion
    return $ object ["access_token" .= tok, "data" .= toData env mhost mcv]
  where
    toData :: Environment -> Maybe Host -> Maybe CodeVersion -> Value
    toData env host cv =
        object $
        [ "environment" .= env
        , "level" .= (toLower <$> show (evt ^. eventLevel))
        , "title" .= (evt ^. eventTitle)
        , "body" .=
          object
              ["message" .= object ["body" .= (evt ^. eventMessage), "data" .= (evt ^. eventData)]]
        , "server" .= object (catMaybes [("host" .=) <$> host])
        , "notifier" .=
          object ["name" .= ("cv-rollbar-haskell" :: Text), "version" .= ("0.2.0" :: Text)]
        ] <>
        catMaybes
            [ ("uuid" .=) <$> evt ^. eventUUID
            , ("context" .=) <$> evt ^. eventContext
            , ("code_version" .=) <$> cv
            ]
