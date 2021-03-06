module GlobalMVar (SyncChannel, new) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Process (Process(..))
import Channel (Channel(..))
import Utils (signalReduction, throwIfBelowSum, delayIfRandom)


data SyncChannel a = Chan (MVar a) (MVar ()) (MVar ())

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send (Chan cont check1 check2) msg (Proc p) = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    putMVar check1 ()
    putMVar cont msg
    takeMVar check2
    takeMVar check1
    signalReduction env
    p env

  recv :: SyncChannel a -> (a -> Process b) -> Process b
  recv (Chan cont _ check2) p = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    msg <- takeMVar cont
    putMVar check2 ()
    signalReduction env
    let Proc p' = p msg
    p' env

new :: (SyncChannel a -> Process b) -> Process b 
new p = Proc \env -> do
  throwIfBelowSum env
  cont <- newEmptyMVar
  check1 <- newEmptyMVar
  check2 <- newEmptyMVar
  let Proc p' = p (Chan cont check1 check2)
  p' env