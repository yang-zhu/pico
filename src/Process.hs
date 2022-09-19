module Process where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TMVar)
import Environment (Environment(..))
import Utils (throwIfBelowSum, signalReduction, delayIfRandom)


-- | A process of type 'Process a' represents a pi-calculus process that might terminate with a result of type 'a'. 
newtype Process a = Proc (Environment a -> IO ())

-- | Execute a process and yield its result. Execution will block until the process has terminated by reaching (stop x), where x is the result of the process.
runProcess :: Process a -> IO a
runProcess (Proc p) = do
  stopVar <- newEmptyMVar
  forkIO $ p Env{stopVar = stopVar, reduced = Nothing, belowSum = Nothing, random = False}
  takeMVar stopVar

-- | 'runProcessRandom' is similar to 'runProcess', but randomizes the execution order by adding random delays before sending or receiving a message.
runProcessRandom :: Process a -> IO a
runProcessRandom (Proc p) = do
  stopVar <- newEmptyMVar
  forkIO $ p Env{stopVar = stopVar, reduced = Nothing, belowSum = Nothing, random = True}
  takeMVar stopVar

-- | Terminate execution and produce the given parameter of type 'a' as the result.
stop :: a -> Process a
stop res = Proc \env@Env{stopVar} -> do
  throwIfBelowSum env
  delayIfRandom env
  putMVar stopVar res

-- | The process that does nothing.
inert :: Process a
inert = Proc \_ -> return ()

-- | Run two processes in parallel.
par :: Process a -> Process a -> Process a
par (Proc p) (Proc q) = Proc \env -> do
  throwIfBelowSum env
  forkIO (q env)
  p env

-- | Replicate the given process as if infinitely many copies of it run in parallel.
-- The process is only replicated when it has interacted with other parts.
repl :: Process a -> Process a
repl (Proc p) = Proc \env -> do
  throwIfBelowSum env
  let loop = forever do
        pReduced <- newEmptyMVar
        forkIO $ p env{reduced = Just pReduced}
        takeMVar pReduced
        signalReduction env
  forkIO loop 
  loop

-- | Replicate the given process as if infinitely many copies of it run in parallel.
-- The process is immediately replicated infinitely often.
alwaysRepl :: Process a -> Process a
alwaysRepl (Proc p) = Proc \env -> do
  throwIfBelowSum env
  forever $ forkIO (p env)

-- | After performing the given action, continue with the execution of the given process.
-- The result produced by the action can be used by the process.
exec :: IO a -> (a -> Process b) -> Process b
exec act p = Proc \env -> do
  throwIfBelowSum env
  delayIfRandom env
  res <- act
  let Proc p' = p res
  p' env

-- | After performing the given action, continue with the execution of the given process.
-- The result produced by the action is ignored.
exec_ :: IO a -> Process b -> Process b
exec_ act (Proc p) = Proc \env -> do
  throwIfBelowSum env
  delayIfRandom env
  act
  p env
