module GlobalTMVar (SyncChannel, new) where

import Data.Maybe (isJust, fromJust)
import Control.Monad (when)
import System.Random (randomRIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TMVar, STM, atomically, newEmptyTMVarIO, takeTMVar, putTMVar)
import Process (Process(..), Environment(..))
import Sum (choose)
import Channel (Channel(..))
import Utils (signalReduction, delayIfRandom, putTMVarIO, takeTMVarIO)


data SyncChannel a = Chan
  { content :: TMVar a 
  , check1 :: TMVar ()
  , check2 :: TMVar ()
  }

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process b -> Process b
  send Chan{content, check1, check2} msg (Proc p) = Proc \env -> do
    delayIfRandom env
    putTMVarIO check1 ()
    putTMVarIO content msg
    takeTMVarIO check2
    takeTMVarIO check1
    signalReduction env
    p env

  recv :: SyncChannel a -> (a -> Process b) -> Process b
  recv Chan{content, check2} p = Proc \env@Env{belowSum} -> do
    delayIfRandom env
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
