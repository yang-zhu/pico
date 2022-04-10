module GlobalMVar where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


data Channel a = Chan (MVar a) (MVar ()) (MVar ())
newtype Process = Proc ((MVar (), MVar Bool) -> IO ())


runProcess :: Process -> IO ()
runProcess (Proc p) = do
  stop <- newMVar ()
  reduced <- newEmptyMVar
  forkIO (p (stop, reduced))
  putMVar stop ()

stop :: Process
stop = Proc \(stop, _) -> takeMVar stop

inert :: Process
inert = Proc \_ -> return ()

par :: Process -> Process -> Process
par (Proc p) (Proc q) = Proc \env -> forkIO (q env) >> p env

repl :: Process -> Process
repl (Proc p) = Proc \(stop, _) ->
  forever do
    hasReduced <- newEmptyMVar
    forkIO (p (stop, hasReduced))
    takeMVar hasReduced

alwaysRepl :: Process -> Process
alwaysRepl (Proc p) = Proc \env -> forever $ forkIO (p env)

new :: (Channel a -> Process) -> Process 
new p = Proc \env -> do
  cont <- newEmptyMVar
  check1 <- newEmptyMVar
  check2 <- newEmptyMVar
  let Proc p' = p (Chan cont check1 check2)
  p' env

output :: Channel a -> a -> Process -> Process
output (Chan cont check1 check2) msg (Proc p) = Proc \env@(_, reduced) -> do
  putMVar check1 ()
  putMVar cont msg
  takeMVar check2
  takeMVar check1
  tryPutMVar reduced True
  p env

input :: Channel a -> (a -> Process) -> Process
input (Chan cont check1 check2) p = Proc \env@(_, reduced) -> do
  msg <- takeMVar cont
  putMVar check2 ()
  tryPutMVar reduced True
  let Proc p' = p msg
  p' env

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc \env -> act >> p env