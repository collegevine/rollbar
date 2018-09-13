{-# LANGUAGE OverloadedStrings #-}

module Web.Rollbar.Internal where

import Control.Lens ((^.), view)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, (.=), object)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Web.Rollbar.Types
    ( CodeVersion
    , Environment
    , Event
    , HasRollbarCfg
    , Host
    , eventContext
    , eventData
    , eventLevel
    , eventMessage
    , eventTitle
    , eventUUID
    , rollbarCfgCodeVersion
    , rollbarCfgEnvironment
    , rollbarCfgHost
    , rollbarCfgToken
    )

-- | Encode an @Event@ Documentation: https://docs.rollbar.com/reference
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
        , "level" .= (evt ^. eventLevel)
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
