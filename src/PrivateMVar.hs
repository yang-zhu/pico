module PrivateMVar (SyncChannel, new) where

import Control.Monad (forM_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)
import Process (Process(..))
import Channel (Channel(..))
import Utils (signalReduction, throwIfBelowSum, delayIfRandom) 


newtype SyncChannel a = Chan (MVar (a, MVar ()))

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send (Chan chan) msg (Proc p) = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    checkChan <- newMVar ()
    putMVar chan (msg, checkChan)
    putMVar checkChan ()
    signalReduction env
    p env

  recv :: SyncChannel a -> (a -> Process b) -> Process b
  recv (Chan chan) p = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    (msg, checkChan) <- takeMVar chan
    takeMVar checkChan
    signalReduction env
    let Proc p' = p msg
    p' env

new :: (SyncChannel a -> Process b) -> Process b 
new p = Proc \env -> do
  throwIfBelowSum env
  chan <- newEmptyMVar
  let Proc p' = p (Chan chan)
  p' env