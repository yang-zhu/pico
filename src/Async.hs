module Async (AsyncChannel, new, chooseMulti) where

import Control.Monad ((>=>) )
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TVar, STM, atomically, retry, orElse, newTVarIO, readTVar, writeTVar, modifyTVar)
import Process (Process(..), Environment(..))
import Channel (ExtendedChannel(..), Channel(..))
import Utils (Queue, isEmpty, enqueue, dequeue)


newtype AsyncChannel a = AsyncChan (TVar (Queue a))

instance Channel AsyncChannel where
  send :: AsyncChannel a -> a -> Process -> Process
  send (AsyncChan chan) msg (Proc p) = Proc \env@Env{reduced} -> do
    atomically $ modifyTVar chan (enqueue msg)
    tryPutMVar reduced ()
    p env
  
  recv :: AsyncChannel a -> (a -> Process) -> Process
  recv chan p = Proc \env -> do
    msg <- atomically $ takeMessage chan
    recvHelper p msg env

instance ExtendedChannel AsyncChannel where
  choose :: AsyncChannel a -> (a -> Process) -> AsyncChannel b -> (b -> Process) -> Process
  choose chan1 p1 chan2 p2 = Proc \env ->
    atomically (orElse (Left <$> takeMessage chan1) (Right <$> takeMessage chan2)) >>= \case
      Left msg -> recvHelper p1 msg env
      Right msg -> recvHelper p2 msg env

takeMessage :: AsyncChannel a -> STM a
takeMessage (AsyncChan chan) = do
  queue <- readTVar chan
  if isEmpty queue
    then retry
    else do
      let (ele, queue') = dequeue queue
      writeTVar chan queue'
      return ele

new :: (AsyncChannel a -> Process) -> Process
new p = Proc \env -> do
  chan <- newTVarIO ([], [])
  let Proc p' = p (AsyncChan chan)
  p' env

recvHelper :: (a -> Process) -> a -> Environment -> IO ()
recvHelper p msg env@Env{reduced} = do
  tryPutMVar reduced ()
  let Proc p' = p msg
  p' env

chooseMulti :: forall a. [AsyncChannel a] -> [a -> Process] -> Process
chooseMulti [] _ = undefined
chooseMulti chans ps = Proc \env -> do
  (msg, p) <- atomically $ foldl1 orElse (map go pairs)
  recvHelper p msg env
  where
    pairs :: [(AsyncChannel a, a -> Process)]
    pairs = zip chans ps

    go :: (AsyncChannel a, a -> Process) -> STM (a, a -> Process)
    go (chan, p) = takeMessage chan >>= \msg -> return (msg, p)
