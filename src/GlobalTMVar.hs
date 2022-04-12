module GlobalTMVar where

import Data.Functor ((<&>))
import Control.Concurrent (forkIO, tryPutMVar)
import Control.Monad (forever)
import Control.Concurrent.STM (TMVar, atomically, orElse, newEmptyTMVar, newEmptyTMVarIO, newTMVarIO, putTMVar, takeTMVar, tryPutTMVar)


data Channel a = Chan
  { content :: TMVar a 
  , check1 :: TMVar ()
  , check2 :: TMVar ()
  }
data Environment = Env
  { stopVar :: TMVar ()
  , reduced :: TMVar ()
  }
newtype Process = Proc (Environment -> IO ())


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
  cont <- newEmptyTMVarIO
  check1 <- newEmptyTMVarIO
  check2 <- newEmptyTMVarIO
  let Proc p' = p (Chan cont check1 check2)
  p' env

send :: Channel a -> a -> Process -> Process
send Chan{content, check1, check2} msg (Proc p) = Proc \env@Env{reduced} -> do
  putTMVarIO check1 ()
  putTMVarIO content msg
  takeTMVarIO check2
  takeTMVarIO check1
  tryPutTMVarIO reduced ()
  p env

recvHelper :: TMVar () -> (a -> Process) -> a -> Environment -> IO ()
recvHelper checkChan p msg env@Env{reduced} = do
  putTMVarIO checkChan ()
  tryPutTMVarIO reduced ()
  let Proc p' = p msg
  p' env

recv :: Channel a -> (a -> Process) -> Process
recv Chan{content, check2} p = Proc \env -> do
  msg <- takeTMVarIO content
  recvHelper check2 p msg env

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc \env -> act >> p env

choose :: Channel a -> (a -> Process) -> Channel b -> (b -> Process) -> Process
choose chan1 p1 chan2 p2 = Proc \env ->
  atomically (orElse (takeTMVar (content chan1) <&> Left) (takeTMVar (content chan2) <&> Right)) >>= \case
    Left msg -> recvHelper (check2 chan1) p1 msg env
    Right msg -> recvHelper (check2 chan2) p2 msg env