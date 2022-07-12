module Utils where

import Control.Monad (forM_)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import Process (Environment(..))


signalReduction :: Environment a -> IO ()
signalReduction Env{reduced} = forM_ reduced (\mvar -> tryPutMVar mvar ())

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO = (atomically .) . putTMVar

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar