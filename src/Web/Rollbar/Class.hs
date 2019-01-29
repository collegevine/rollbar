module Web.Rollbar.Class
    ( MonadRollbarWriter(..)
    ) where

import qualified Web.Rollbar as R
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Network.HTTP.Nano
    ( AsHttpError
    , HasHttpCfg
    )

class MonadRollbarWriter m where
    rollbar :: (R.ToRollbarEvent evt) => evt -> m ()

instance 
    ( MonadIO m
    , MonadError e m
    , MonadReader r m
    , AsHttpError e
    , HasHttpCfg r
    , R.HasRollbarCfg r
    ) => MonadRollbarWriter m where
    rollbar = R.rollbar
