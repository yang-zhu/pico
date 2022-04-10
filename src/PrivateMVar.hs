module PrivateMVar where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


newtype Channel a = Chan (MVar (a, MVar ()))
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
  chan <- newEmptyMVar
  let Proc p' = p (Chan chan)
  p' env

output :: Channel a -> a -> Process -> Process
output (Chan chan) msg (Proc p) = Proc \env@(_, reduced) -> do
  checkChan <- newMVar ()
  putMVar chan (msg, checkChan)
  putMVar checkChan ()
  tryPutMVar reduced True
  p env

input :: Channel a -> (a -> Process) -> Process
input (Chan chan) p = Proc \env@(_, reduced) -> do
  (msg, checkChan) <- takeMVar chan
  takeMVar checkChan
  tryPutMVar reduced True
  let Proc p' = p msg
  p' env

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc \env -> act >> p env