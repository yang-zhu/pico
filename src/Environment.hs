module Environment where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TMVar)


-- | The 'Environment' contains the information that is required to execute a process.
data Environment a = Env
  { stopVar :: MVar a            -- ^ Writing the result into 'stopVar' signals termination.
  , reduced :: Maybe (MVar ())   -- ^ Only used below 'repl'. Filling 'reduced' signals 'repl' to create a new copy of the process.
  , belowSum :: Maybe (TMVar ()) -- ^ Only used below 'choose'. Filling 'belowSum' renders the capability of the other summand void.
  , random :: Bool               -- ^ Enabled when 'runProcessRandom' is used.
  }