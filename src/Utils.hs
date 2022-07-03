module Utils where

import Control.Monad (forM_)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import Process (Environment(..))


type Queue a = ([a], [a])

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


signalReduction :: Environment a -> IO ()
signalReduction Env{reduced} = forM_ reduced (\mvar -> tryPutMVar mvar ())

putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO = (atomically .) . putTMVar

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar