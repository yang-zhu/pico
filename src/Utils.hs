module Utils where

import Control.Monad (forM_, when)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import Environment (Environment(..))


-- | 'signalReduction' is used to inform 'repl p' to create a new copy of 'p'.
signalReduction :: Environment a -> IO ()
signalReduction Env{reduced} = forM_ reduced (`tryPutMVar` ())

-- | An error is thrown when an unsupported combinator is used as an operand of 'choose'.
throwIfBelowSum :: Environment a -> IO ()
throwIfBelowSum Env{belowSum} = forM_  belowSum (error "cannot be operand of choose")

-- | When using 'runProcessRandom', we add random delays before send and receive operations.
delayIfRandom :: Environment a -> IO ()
delayIfRandom Env{random} = when random (randomRIO (0, 5000) >>= threadDelay)

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO = (atomically .) . putTMVar

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar