module GlobalMVar where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)


data Channel a = Chan (MVar a) (MVar ()) (MVar ())
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
  cont <- newEmptyMVar
  check1 <- newEmptyMVar
  check2 <- newEmptyMVar
  let Proc p' = p (Chan cont check1 check2)
  p' stop

output :: Channel a -> a -> Process -> Process
output (Chan cont check1 check2) msg (Proc p) = Proc $ \stop -> do
  putMVar check1 ()
  putMVar cont msg
  takeMVar check2
  takeMVar check1
  p stop

input :: Channel a -> (a -> Process) -> Process
input (Chan cont check1 check2) p = Proc $ \stop -> do
  msg <- takeMVar cont
  putMVar check2 ()
  let Proc p' = p msg
  p' stop

exec :: IO a -> Process -> Process
exec act (Proc p) = Proc $ \stop -> act >> p stop