{-
This module defines the sychronous pi-calculus channel type and combinators that involve communication between processes.

The definitions follow the Global MVars approach introduced in the paper
Manfred Schmidt-Schauß and David Sabel. Correctly implementing synchronous message passing in the pi-calculus by Concurrent Haskell’s MVars.
In Ornela Dardha and Jurriaan Rot, editors, Proceedings Combined 27th International Workshop on Expressiveness in Concurrency and 17th Workshop on Structural Operational Semantics, EXPRESS/SOS 2020, volume 322 of EPTCS, pages 88–105, 2020.
-}

module GlobalMVar (SyncChannel, new) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Process (Process(..))
import Channel (Channel(..))
import Utils (signalReduction, throwIfBelowSum, delayIfRandom)


-- | A synchronous pi-calculus channel.
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

-- | Create a new synchronous channel and proceed to the execution of the given process.
new :: (SyncChannel a -> Process b) -> Process b 
new p = Proc \env -> do
  throwIfBelowSum env
  cont <- newEmptyMVar
  check1 <- newEmptyMVar
  check2 <- newEmptyMVar
  let Proc p' = p (Chan cont check1 check2)
  p' env