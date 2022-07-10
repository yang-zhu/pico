module PrivateTMVar (SyncChannel, new, choose) where

import Control.Concurrent.MVar (tryPutMVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM (STM, TMVar, atomically, orElse, newEmptyTMVarIO, newTMVarIO, takeTMVar)
import Process (Process(..), Environment(..))
import Channel (Channel(..))
import Utils (signalReduction, putTMVarIO, takeTMVarIO)
import Control.Concurrent (forkIO)
import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import Control.Concurrent.STM.TMVar (putTMVar)


newtype SyncChannel a = Chan (TMVar (a, TMVar ()))

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send (Chan chan) msg (Proc p) = Proc \env -> do
    checkChan <- newTMVarIO ()
    putTMVarIO chan (msg, checkChan)
    putTMVarIO checkChan ()
    signalReduction env
    p env

  recv :: SyncChannel a -> (a -> Process b) -> Process b
  recv (Chan chan) p = Proc \env@Env{belowSum} -> do
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

choose :: Process a -> Process a -> Process a
choose (Proc p1) (Proc p2) = Proc \env@Env{belowSum} -> do
  progress <- maybe newEmptyTMVarIO return belowSum
  forkIO $ p2 env{belowSum = Just progress}
  p1 env{belowSum = Just progress}
