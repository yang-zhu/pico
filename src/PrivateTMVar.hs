module PrivateTMVar (SyncChannel, new, chooseMulti) where

import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (STM, TMVar, atomically, orElse, newEmptyTMVarIO, newTMVarIO, takeTMVar)
import Process (Process(..), Environment(..))
import Channel (ExtendedChannel(..), Channel(..))
import Utils (signalReduction, putTMVarIO, takeTMVarIO)


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
  recv (Chan chan) p = Proc \env -> do
    (msg, checkChan) <- takeTMVarIO chan
    recvHelper checkChan p msg env
  
instance ExtendedChannel SyncChannel where
  choose :: SyncChannel a -> (a -> Process c) -> SyncChannel b -> (b -> Process c) -> Process c
  choose (Chan chan1) p1 (Chan chan2) p2 = Proc \env ->
    atomically (orElse (Left <$> takeTMVar chan1) (Right <$> takeTMVar chan2))
    >>= \case
      Left (msg, checkChan) -> recvHelper checkChan p1 msg env
      Right (msg, checkChan) -> recvHelper checkChan p2 msg env

new :: (SyncChannel a -> Process b) -> Process b 
new p = Proc \env -> do
  chan <- newEmptyTMVarIO
  let Proc p' = p (Chan chan)
  p' env

recvHelper :: TMVar () -> (a -> Process b) -> a -> Environment b -> IO ()
recvHelper checkChan p msg env = do
  takeTMVarIO checkChan
  signalReduction env
  let Proc p' = p msg
  p' env

chooseMulti :: forall a b. [SyncChannel a] -> [a -> Process b] -> Process b
chooseMulti [] _ = undefined
chooseMulti chans ps = Proc \env -> do
  ((msg, checkChan), p) <- atomically $ foldl1 orElse (map go pairs)
  recvHelper checkChan p msg env
  where
    pairs :: [(SyncChannel a, a -> Process b)]
    pairs = zip chans ps

    go :: (SyncChannel a, a -> Process b) -> STM ((a, TMVar ()), a -> Process b)
    go (Chan chan, p) = takeTMVar chan >>= \res -> return (res, p)
