module Process where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TMVar)
import Environment (Environment(..))
import Utils (throwIfBelowSum)


newtype Process a = Proc (Environment a -> IO ())


runProcess :: Process a -> IO a
runProcess (Proc p) = do
  stop <- newEmptyMVar
  forkIO $ p (Env stop Nothing Nothing False)
  takeMVar stop

runProcessRandom :: Process a -> IO a
runProcessRandom (Proc p) = do
  stop <- newEmptyMVar
  forkIO $ p (Env stop Nothing Nothing True)
  takeMVar stop

stop :: a -> Process a
stop res = Proc \env@Env{stopVar} -> do
  throwIfBelowSum env
  putMVar stopVar res

inert :: Process a
inert = Proc \_ -> return ()

par :: Process a -> Process a -> Process a
par (Proc p) (Proc q) = Proc \env -> do
  throwIfBelowSum env
  forkIO (q env) >> p env

-- process p is only replicated when it has been reduced
repl :: Process a -> Process a
repl (Proc p) = Proc \env -> do
  throwIfBelowSum env
  forever do
    pReduced <- newEmptyMVar
    forkIO $ p env{reduced = Just pReduced}
    takeMVar pReduced

-- process p is immediately replicated infinitely often
alwaysRepl :: Process a -> Process a
alwaysRepl (Proc p) = Proc \env -> do
  throwIfBelowSum env
  forever $ forkIO (p env)

exec :: IO a -> (a -> Process b) -> Process b
exec act p = Proc \env -> do
  throwIfBelowSum env
  res <- act
  let Proc p' = p res
  p' env

exec_ :: IO a -> Process b -> Process b
exec_ act (Proc p) = Proc \env -> do
  throwIfBelowSum env
  act
  p env
