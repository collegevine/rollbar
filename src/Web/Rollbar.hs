{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar(
    module Web.Rollbar.Types,
    rollbar
) where

import Web.Rollbar.Types

import Control.Lens (view, (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import Network.HTTP.Nano

rollbar :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r, HasRollbarCfg r) => Event -> m ()
rollbar evt = do
    v <- encodeEvent evt
    http' . addHeaders [("Content-Type", "application/json")] =<< buildReq POST "https://api.rollbar.com/api/1/item/" (mkJSONData v)

encodeEvent :: (MonadReader r m, HasRollbarCfg r) => Event -> m Value
encodeEvent evt = do
    tok <- view rollbarCfgToken
    env <- view rollbarCfgEnvironment
    return $ object ["access_token" .= tok, "data" .= dta env]
    where
    dta env =
        object $ [
            "environment" .= env,
            "level" .= (toLower <$> show (evt ^. eventLevel)),
            "title" .= (evt ^. eventTitle),
            "body" .= object ["message" .= object ["body" .= (evt ^. eventMessage), "data" .= (evt ^. eventData)]]
        ] ++ maybeToList (("uuid".=) <$> evt ^. eventUUID)
