module PrivateMVar where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad (forever)


newtype Channel a = Chan (MVar (a, MVar ()))
newtype Process = Proc (MVar () -> IO ())


runProcess :: Process -> IO ()
runProcess (Proc p) = do
  stop <- newMVar ()
  forkIO (p stop)
  putMVar stop ()

stop :: Process
stop = Proc takeMVar

inert :: Process
inert = Proc $ \_ -> return ()

par :: Process -> Process -> Process
par (Proc p) (Proc q) = Proc $ \stop -> forkIO (q stop) >> p stop

repl :: Process -> Process
repl (Proc p) = Proc $ \stop -> let p' = forkIO (p stop) >> p' in p'

new :: (Channel a -> Process) -> Process 
new p = Proc $ \stop -> do
  chan <- newEmptyMVar
  let Proc p' = p (Chan chan)
  p' stop

output :: Channel a -> a -> Process -> Process
output (Chan chan) msg (Proc p) = Proc $ \stop -> do
  checkChan <- newMVar ()
  putMVar chan (msg, checkChan)
  putMVar checkChan ()
  p stop

input :: Channel a -> (a -> Process) -> Process
input (Chan chan) p = Proc $ \stop -> do
  (msg, checkChan) <- takeMVar chan
  takeMVar checkChan
  let Proc p' = p msg
  p' stop

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc $ \stop -> act >> p stop