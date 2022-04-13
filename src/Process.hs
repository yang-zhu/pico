module Process where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)

data Environment = Env
  { stopVar :: MVar ()
  , reduced :: MVar ()
  }
newtype Process = Proc (Environment -> IO ())


runProcess :: Process -> IO ()
runProcess (Proc p) = do
  stop <- newMVar ()
  reduced <- newEmptyMVar
  forkIO $ p (Env stop reduced)
  putMVar stop ()

stop :: Process
stop = Proc \Env{stopVar} -> takeMVar stopVar

inert :: Process
inert = Proc \_ -> return ()

par :: Process -> Process -> Process
par (Proc p) (Proc q) = Proc \env -> forkIO (q env) >> p env

-- process p is only replicated when it has been reduced
repl :: Process -> Process
repl (Proc p) = Proc \Env{stopVar} ->
  forever do
    pReduced <- newEmptyMVar
    forkIO $ p (Env stopVar pReduced)
    takeMVar pReduced

-- process p is immediately replicated infinitely often
alwaysRepl :: Process -> Process
alwaysRepl (Proc p) = Proc \env -> forever $ forkIO (p env)

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc \env -> act >> p env