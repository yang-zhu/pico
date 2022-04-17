module GlobalTMVar (SyncChannel, new, chooseMulti) where

import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, STM, atomically, orElse, newEmptyTMVarIO, takeTMVar)
import Process (Process(..), Environment(..))
import Channel (ExtendedChannel(..), Channel(..))
import Utils (putTMVarIO, takeTMVarIO)


data SyncChannel a = Chan
  { content :: TMVar a 
  , check1 :: TMVar ()
  , check2 :: TMVar ()
  }

instance Channel SyncChannel where
  send :: SyncChannel a -> a -> Process -> Process
  send Chan{content, check1, check2} msg (Proc p) = Proc \env@Env{reduced} -> do
    putTMVarIO check1 ()
    putTMVarIO content msg
    takeTMVarIO check2
    takeTMVarIO check1
    tryPutMVar reduced ()
    p env

  recv :: SyncChannel a -> (a -> Process) -> Process
  recv Chan{content, check2} p = Proc \env -> do
    msg <- takeTMVarIO content
    recvHelper check2 p msg env

instance ExtendedChannel SyncChannel where
  choose :: SyncChannel a -> (a -> Process) -> SyncChannel b -> (b -> Process) -> Process
  choose chan1 p1 chan2 p2 = Proc \env ->
    atomically (orElse (Left <$> takeTMVar (content chan1)) (Right <$> takeTMVar (content chan2)))
    >>= \case
      Left msg -> recvHelper (check2 chan1) p1 msg env
      Right msg -> recvHelper (check2 chan2) p2 msg env

new :: (SyncChannel a -> Process) -> Process 
new p = Proc \env -> do
  cont <- newEmptyTMVarIO
  check1 <- newEmptyTMVarIO
  check2 <- newEmptyTMVarIO
  let Proc p' = p (Chan cont check1 check2)
  p' env

recvHelper :: TMVar () -> (a -> Process) -> a -> Environment -> IO ()
recvHelper checkChan p msg env@Env{reduced} = do
  putTMVarIO checkChan ()
  tryPutMVar reduced ()
  let Proc p' = p msg
  p' env

chooseMulti :: forall a. [SyncChannel a] -> [a -> Process] -> Process
chooseMulti [] _ = undefined
chooseMulti chans ps = Proc \env -> do
  (msg, chan, p) <- atomically $ foldl1 orElse (map go pairs)
  recvHelper (check2 chan) p msg env
  where
    pairs :: [(SyncChannel a, a -> Process)]
    pairs = zip chans ps

    go :: (SyncChannel a, a -> Process) -> STM (a, SyncChannel a, a -> Process)
    go (chan, p) = takeTMVar (content chan) >>= \msg -> return (msg, chan, p)