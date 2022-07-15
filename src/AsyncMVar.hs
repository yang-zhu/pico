module AsyncMVar (AsyncChannel, new) where

import Control.Concurrent (Chan, writeChan, readChan, newChan)
import Environment (Environment(..))
import Process (Process(..))
import Channel (Channel(..))
import Utils (signalReduction, throwIfBelowSum, delayIfRandom)


newtype AsyncChannel a = AsyncChan (Chan a)


instance Channel AsyncChannel where
  send :: AsyncChannel a -> a -> Process b -> Process b
  send (AsyncChan chan) msg (Proc p) = Proc \env -> do
    throwIfBelowSum env
    delayIfRandom env
    writeChan chan msg
    signalReduction env
    p env
  
  recv :: AsyncChannel a -> (a -> Process b) -> Process b
  recv (AsyncChan chan) p = Proc \env@Env{belowSum} -> do
    throwIfBelowSum env
    delayIfRandom env
    msg <- readChan chan
    signalReduction env
    let Proc p' = p msg
    p' env{belowSum = Nothing}

new :: (AsyncChannel a -> Process b) -> Process b
new p = Proc \env -> do
  throwIfBelowSum env
  chan <- newChan
  let Proc p' = p (AsyncChan chan)
  p' env
