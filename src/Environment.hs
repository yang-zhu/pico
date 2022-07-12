module Environment where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TMVar)

data Environment a = Env
  { stopVar :: MVar a
  , reduced :: Maybe (MVar ())
  , belowSum :: Maybe (TMVar ())
  , random :: Bool
  }