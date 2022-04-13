module Utils where

import Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)


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


putTMVarIO :: TMVar a -> a -> IO ()
putTMVarIO = (atomically .) . putTMVar

takeTMVarIO :: TMVar a -> IO a
takeTMVarIO = atomically . takeTMVar