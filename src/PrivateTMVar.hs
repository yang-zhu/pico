module PrivateTMVar (SyncChannel) where

import Data.Functor ((<&>))
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, atomically, orElse, newEmptyTMVarIO, newTMVarIO, takeTMVar)
import Process (Process(..), Environment(..))
import Channel (ExtendedChannel(..), Channel(..))
import Utils (putTMVarIO, takeTMVarIO)


newtype SyncChannel a = Chan (TMVar (a, TMVar ()))

instance Channel SyncChannel where
  new :: (SyncChannel a -> Process) -> Process 
  new p = Proc \env -> do
    chan <- newEmptyTMVarIO
    let Proc p' = p (Chan chan)
    p' env

  send :: SyncChannel a -> a -> Process -> Process
  send (Chan chan) msg (Proc p) = Proc \env@Env{reduced} -> do
    checkChan <- newTMVarIO ()
    putTMVarIO chan (msg, checkChan)
    putTMVarIO checkChan ()
    tryPutMVar reduced ()
    p env

  recv :: SyncChannel a -> (a -> Process) -> Process
  recv (Chan chan) p = Proc \env -> do
    (msg, checkChan) <- takeTMVarIO chan
    recvHelper checkChan p msg env
  
instance ExtendedChannel SyncChannel where
  choose :: SyncChannel a -> (a -> Process) -> SyncChannel b -> (b -> Process) -> Process
  choose (Chan chan1) p1 (Chan chan2) p2 = Proc \env ->
    atomically (orElse (takeTMVar chan1 <&> Left) (takeTMVar chan2 <&> Right))
    >>= \case
      Left (msg, checkChan) -> recvHelper checkChan p1 msg env
      Right (msg, checkChan) -> recvHelper checkChan p2 msg env

recvHelper :: TMVar () -> (a -> Process) -> a -> Environment -> IO ()
recvHelper checkChan p msg env@Env{reduced} = do
  takeTMVarIO checkChan
  tryPutMVar reduced ()
  let Proc p' = p msg
  p' env