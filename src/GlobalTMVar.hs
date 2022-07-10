module GlobalTMVar (SyncChannel, new, choose) where

import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, STM, atomically, orElse, newEmptyTMVarIO, takeTMVar)
import Process (Process(..), Environment(..))
import Channel (Channel(..))
import Utils (signalReduction, putTMVarIO, takeTMVarIO)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Concurrent (forkIO)


data SyncChannel a = Chan
  { content :: TMVar a 
  , check1 :: TMVar ()
  , check2 :: TMVar ()
  }

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send Chan{content, check1, check2} msg (Proc p) = Proc \env -> do
    putTMVarIO check1 ()
    putTMVarIO content msg
    takeTMVarIO check2
    takeTMVarIO check1
    signalReduction env
    p env

  recv :: SyncChannel a -> (a -> Process b) -> Process b
  recv Chan{content, check2} p = Proc \env@Env{belowSum} -> do
    msg <- atomically do
      when (isJust belowSum) (putTMVar (fromJust belowSum) ())
      msg <- takeTMVar content
      putTMVar check2 ()
      return msg
    signalReduction env
    let Proc p' = p msg
    p' env{belowSum = Nothing}

new :: (SyncChannel a -> Process b) -> Process b 
new p = Proc \env -> do
  cont <- newEmptyTMVarIO
  check1 <- newEmptyTMVarIO
  check2 <- newEmptyTMVarIO
  let Proc p' = p (Chan cont check1 check2)
  p' env

choose :: Process a -> Process a -> Process a
choose (Proc p1) (Proc p2) = Proc \env@Env{belowSum} -> do
  progress <- maybe newEmptyTMVarIO return belowSum
  forkIO $ p2 env{belowSum = Just progress}
  p1 env{belowSum = Just progress}
