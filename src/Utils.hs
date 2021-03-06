module Utils where

import Control.Monad (forM_, when)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import Environment (Environment(..))


signalReduction :: Environment a -> IO ()
signalReduction Env{reduced} = forM_ reduced (\mvar -> tryPutMVar mvar ())

throwIfBelowSum :: Environment a -> IO ()
throwIfBelowSum Env{belowSum} = forM_  belowSum (error "cannot be operand of sum operator")

delayIfRandom :: Environment a -> IO ()
delayIfRandom Env{random} = when random (randomRIO (0, 5000) >>= threadDelay)

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO = (atomically .) . putTMVar

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar