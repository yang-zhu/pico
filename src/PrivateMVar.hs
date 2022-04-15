module PrivateMVar (SyncChannel, new) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)
import Process (Process(..), Environment(..))
import Channel (Channel(..))


newtype SyncChannel a = Chan (MVar (a, MVar ()))

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process -> Process
  send (Chan chan) msg (Proc p) = Proc \env@Env{reduced} -> do
    checkChan <- newMVar ()
    putMVar chan (msg, checkChan)
    putMVar checkChan ()
    tryPutMVar reduced ()
    p env

  recv :: SyncChannel a -> (a -> Process) -> Process
  recv (Chan chan) p = Proc \env@Env{reduced} -> do
    (msg, checkChan) <- takeMVar chan
    takeMVar checkChan
    tryPutMVar reduced ()
    let Proc p' = p msg
    p' env

new :: (SyncChannel a -> Process) -> Process 
new p = Proc \env -> do
  chan <- newEmptyMVar
  let Proc p' = p (Chan chan)
  p' env