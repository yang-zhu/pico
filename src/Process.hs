module Process where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)

data Environment a = Env
  { stopVar :: MVar a
  , reduced :: MVar ()
  }
newtype Process a = Proc (Environment a -> IO ())


runProcess :: Process a -> IO a
runProcess (Proc p) = do
  stop <- newEmptyMVar
  reduced <- newEmptyMVar
  forkIO $ p (Env stop reduced)
  takeMVar stop

stop :: a -> Process a
stop res = Proc \Env{stopVar} -> putMVar stopVar res

inert :: Process a
inert = Proc \_ -> return ()

par :: Process a -> Process a -> Process a
par (Proc p) (Proc q) = Proc \env -> forkIO (q env) >> p env

-- process p is only replicated when it has been reduced
repl :: Process a -> Process a
repl (Proc p) = Proc \Env{stopVar} ->
  forever do
    pReduced <- newEmptyMVar
    forkIO $ p (Env stopVar pReduced)
    takeMVar pReduced

-- process p is immediately replicated infinitely often
alwaysRepl :: Process a -> Process a
alwaysRepl (Proc p) = Proc \env -> forever $ forkIO (p env)

exec :: IO a -> Process b -> Process b
exec act (Proc p) = Proc \env -> act >> p env