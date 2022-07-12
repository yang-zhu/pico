module PrivateTMVar (SyncChannel, new) where

import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, newTMVarIO, takeTMVar, putTMVar)
import Process (Process(..), Environment(..))
import Sum (choose)
import Channel (Channel(..))
import Utils (signalReduction, delayIfRandom, putTMVarIO, takeTMVarIO)


newtype SyncChannel a = Chan (TMVar (a, TMVar ()))

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send (Chan chan) msg (Proc p) = Proc \env -> do
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

new :: (SyncChannel a -> Process b) -> Process b
new p = Proc \env -> do
  chan <- newEmptyTMVarIO
  let Proc p' = p (Chan chan)
  p' env
