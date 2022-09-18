module AsyncSTM (AsyncChannel, new) where

import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Control.Concurrent.STM (atomically, putTMVar)
import Control.Concurrent.STM.TChan (TChan, writeTChan, readTChan, newTChanIO)
import Environment (Environment(..))
import Process (Process(..))
import Channel (Channel(..))
import Utils (signalReduction, throwIfBelowSum, delayIfRandom)


-- | An asynchronous pi-calculus channel.
newtype AsyncChannel a = AsyncChan (TChan a)

instance Channel AsyncChannel where
  send :: AsyncChannel a -> a -> Process b -> Process b
  send (AsyncChan chan) msg (Proc p) = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    atomically $ writeTChan chan msg
    signalReduction env
    p env
  
  recv :: AsyncChannel a -> (a -> Process b) -> Process b
  recv (AsyncChan chan) p = Proc \env@Env{belowSum} -> do
    delayIfRandom env
    msg <- atomically do
      when (isJust belowSum) (putTMVar (fromJust belowSum) ())
      readTChan chan
    signalReduction env
    let Proc p' = p msg
    p' env{belowSum = Nothing}

-- | Create a new asynchronous channel and proceed to the execution of the given process.
new :: (AsyncChannel a -> Process b) -> Process b
new p = Proc \env -> do
  throwIfBelowSum env
  chan <- newTChanIO
  let Proc p' = p (AsyncChan chan)
  p' env
