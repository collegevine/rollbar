{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar
    ( module Web.Rollbar.Types
    , rollbar
    ) where

import qualified Web.Rollbar.Internal as I
import Web.Rollbar.Types

import Control.Lens (view)
import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
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
        v <- I.encodeEvent $ toRollbarEvent evt
        http' . addHeaders [("Content-Type", "application/json")] =<<
            buildReq POST "https://api.rollbar.com/api/1/item/" (mkJSONData v)
