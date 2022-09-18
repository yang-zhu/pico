module PrivateTMVar (SyncChannel, new) where

import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, newTMVarIO, takeTMVar, putTMVar)
import Environment (Environment(..))
import Process (Process(..))
import Channel (Channel(..))
import Utils (signalReduction, throwIfBelowSum, delayIfRandom, putTMVarIO)


-- | A synchronous pi-calculus channel.
newtype SyncChannel a = Chan (TMVar (a, TMVar ()))

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send (Chan chan) msg (Proc p) = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    checkChan <- newTMVarIO ()
    putTMVarIO chan (msg, checkChan)
    putTMVarIO checkChan ()
    signalReduction env
    p env

  recv :: SyncChannel a -> (a -> Process b) -> Process b
  recv (Chan chan) p = Proc \env@Env{belowSum} -> do
    delayIfRandom env
    msg <- atomically do
      when (isJust belowSum) (putTMVar (fromJust belowSum) ())
      (msg, checkChan) <- takeTMVar chan
      takeTMVar checkChan
      return msg
    signalReduction env
    let Proc p' = p msg
    p' env{belowSum = Nothing}

-- | Create a new synchronous channel and proceed to the execution of the given process.
new :: (SyncChannel a -> Process b) -> Process b
new p = Proc \env -> do
  throwIfBelowSum env
  chan <- newEmptyTMVarIO
  let Proc p' = p (Chan chan)
  p' env
