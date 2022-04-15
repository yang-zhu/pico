module GlobalMVar (SyncChannel, new) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Process (Process(..), Environment(..))
import Channel (Channel(..))


data SyncChannel a = Chan (MVar a) (MVar ()) (MVar ())

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process -> Process
  send (Chan cont check1 check2) msg (Proc p) = Proc \env@Env{reduced} -> do
    putMVar check1 ()
    putMVar cont msg
    takeMVar check2
    takeMVar check1
    tryPutMVar reduced ()
    p env

  recv :: SyncChannel a -> (a -> Process) -> Process
  recv (Chan cont _ check2) p = Proc \env@Env{reduced} -> do
    msg <- takeMVar cont
    putMVar check2 ()
    tryPutMVar reduced ()
    let Proc p' = p msg
    p' env

new :: (SyncChannel a -> Process) -> Process 
new p = Proc \env -> do
  cont <- newEmptyMVar
  check1 <- newEmptyMVar
  check2 <- newEmptyMVar
  let Proc p' = p (Chan cont check1 check2)
  p' env