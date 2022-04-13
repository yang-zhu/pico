module Async (AsyncChannel) where

import Data.Functor ((<&>))
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TVar, STM, atomically, retry, orElse, newTVarIO, readTVar, writeTVar, modifyTVar)
import Process (Process(..), Environment(..))
import Channel (ExtendedChannel(..), Channel(..))
import Utils (Queue, isEmpty, enqueue, dequeue)


newtype AsyncChannel a = AsyncChan (TVar (Queue a))

instance Channel AsyncChannel where
  new :: (AsyncChannel a -> Process) -> Process
  new p = Proc \env -> do
    chan <- newTVarIO ([], [])
    let Proc p' = p (AsyncChan chan)
    p' env
  
  send :: AsyncChannel a -> a -> Process -> Process
  send (AsyncChan chan) msg (Proc p) = Proc \env@Env{reduced} -> do
    atomically $ modifyTVar chan (enqueue msg)
    tryPutMVar reduced ()
    p env
  
  recv :: AsyncChannel a -> (a -> Process) -> Process
  recv (AsyncChan chan) p = Proc \env -> do
    msg <- atomically $ readTVar chan >>= modifyChan chan
    recvHelper p msg env

instance ExtendedChannel AsyncChannel where
  choose :: AsyncChannel a -> (a -> Process) -> AsyncChannel b -> (b -> Process) -> Process
  choose (AsyncChan chan1) p1 (AsyncChan chan2) p2 = Proc \env ->
    atomically (orElse ((readTVar chan1 >>= modifyChan chan1) <&> Left) ((readTVar chan2 >>= modifyChan chan2) <&> Right)) >>= \case
      Left msg -> recvHelper p1 msg env
      Right msg -> recvHelper p2 msg env

modifyChan :: TVar (Queue a) -> Queue a -> STM a
modifyChan chan queue = do
  if isEmpty queue
    then retry
    else do
      let (ele, queue') = dequeue queue
      writeTVar chan queue'
      return ele

recvHelper :: (a -> Process) -> a -> Environment -> IO ()
recvHelper p msg env@Env{reduced} = do
  tryPutMVar reduced ()
  let Proc p' = p msg
  p' env
