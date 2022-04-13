module PrivateTMVar where

import Data.Functor ((<&>))
import Control.Concurrent (forkIO, tryPutMVar)
import Control.Monad (forever)
import Control.Concurrent.STM (TMVar, TVar, STM, atomically, retry, orElse, newEmptyTMVar, newEmptyTMVarIO, newTMVarIO, putTMVar, takeTMVar, tryPutTMVar, newTVarIO, readTVar, writeTVar, modifyTVar)


newtype Channel a = Chan (TMVar (a, TMVar ()))

type Queue a = ([a], [a])
newtype AsyncChannel a = AsyncChan (TVar (Queue a))

data Environment = Env
  { stopVar :: TMVar ()
  , reduced :: TMVar ()
  }
newtype Process = Proc (Environment -> IO ())

isEmpty :: Queue a -> Bool
isEmpty (enq, deq) = null enq && null deq

enqueue :: a -> Queue a -> Queue a
enqueue x (enq, deq) = (x:enq, deq)

dequeue :: Queue a -> (a, Queue a)
dequeue (enq, deq)
  | null deq = let
      deq' = reverse enq
      in (head deq', ([], tail deq'))
  | otherwise = (head deq, (enq, tail deq))


putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO = (atomically .) . putTMVar

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar

tryPutTMVarIO :: TMVar a -> a -> IO Bool
tryPutTMVarIO = (atomically .). tryPutTMVar

runProcess :: Process -> IO ()
runProcess (Proc p) = do
  stop <- newTMVarIO ()
  reduced <- newEmptyTMVarIO
  forkIO $ p (Env stop reduced)
  putTMVarIO stop ()

stop :: Process
stop = Proc \Env{stopVar} -> takeTMVarIO stopVar

inert :: Process
inert = Proc \_ -> return ()

par :: Process -> Process -> Process
par (Proc p) (Proc q) = Proc \env -> forkIO (q env) >> p env

-- process p is only replicated when it has been reduced
repl :: Process -> Process
repl (Proc p) = Proc \Env{stopVar} ->
  forever do
    pReduced <- newEmptyTMVarIO
    forkIO $ p (Env stopVar pReduced)
    takeTMVarIO pReduced

-- process p is immediately replicated infinitely often
alwaysRepl :: Process -> Process
alwaysRepl (Proc p) = Proc \env -> forever $ forkIO (p env)

new :: (Channel a -> Process) -> Process 
new p = Proc \env -> do
  chan <- newEmptyTMVarIO
  let Proc p' = p (Chan chan)
  p' env

newAsync :: (AsyncChannel a -> Process) -> Process
newAsync p = Proc \env -> do
  chan <- newTVarIO ([], [])
  let Proc p' = p (AsyncChan chan)
  p' env

send :: Channel a -> a -> Process -> Process
send (Chan chan) msg (Proc p) = Proc \env@Env{reduced} -> do
  checkChan <- newTMVarIO ()
  putTMVarIO chan (msg, checkChan)
  putTMVarIO checkChan ()
  tryPutTMVarIO reduced ()
  p env

sendAsync :: AsyncChannel a -> a -> Process -> Process
sendAsync (AsyncChan chan) msg (Proc p) = Proc \env@Env{reduced} -> do
  atomically $ modifyTVar chan (enqueue msg)
  tryPutTMVarIO reduced ()
  p env

recvHelper :: TMVar () -> (a -> Process) -> a -> Environment -> IO ()
recvHelper checkChan p msg env@Env{reduced} = do
  takeTMVarIO checkChan
  tryPutTMVarIO reduced ()
  let Proc p' = p msg
  p' env

recv :: Channel a -> (a -> Process) -> Process
recv (Chan chan) p = Proc \env -> do
  (msg, checkChan) <- takeTMVarIO chan
  recvHelper checkChan p msg env

modifyAsyncChan :: TVar (Queue a) -> Queue a -> STM a
modifyAsyncChan chan queue = do
  if isEmpty queue
    then retry
    else do
      let (ele, queue') = dequeue queue
      writeTVar chan queue'
      return ele

recvAsyncHelper :: (a -> Process) -> a -> Environment -> IO ()
recvAsyncHelper p msg env@Env{reduced} = do
  tryPutTMVarIO reduced ()
  let Proc p' = p msg
  p' env

recvAsync :: AsyncChannel a -> (a -> Process) -> Process
recvAsync (AsyncChan chan) p = Proc \env -> do
  msg <- atomically $ readTVar chan >>= modifyAsyncChan chan
  recvAsyncHelper p msg env

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc \env -> act >> p env

choose :: Channel a -> (a -> Process) -> Channel b -> (b -> Process) -> Process
choose (Chan chan1) p1 (Chan chan2) p2 = Proc \env ->
  atomically (orElse (takeTMVar chan1 <&> Left) (takeTMVar chan2 <&> Right))
  >>= \case
    Left (msg, checkChan) -> recvHelper checkChan p1 msg env
    Right (msg, checkChan) -> recvHelper checkChan p2 msg env

chooseAsync :: AsyncChannel a -> (a -> Process) -> AsyncChannel b -> (b -> Process) -> Process
chooseAsync (AsyncChan chan1) p1 (AsyncChan chan2) p2 = Proc \env ->
  atomically (orElse ((readTVar chan1 >>= modifyAsyncChan chan1) <&> Left) ((readTVar chan2 >>= modifyAsyncChan chan2) <&> Right))
  >>= \case
    Left msg -> recvAsyncHelper p1 msg env
    Right msg -> recvAsyncHelper p2 msg env
