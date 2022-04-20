import Process
import Channel
import PiQQ
-- import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
import GlobalTMVar
-- import Async

main :: IO ()
main = runProcess $ 
  new (\finalRes -> new (\fib ->
    send fib (20, finalRes) inert `par`
    recv finalRes (\(r::Integer) -> exec (print r) stop) `par`
    fibN fib
  ))
  where
    fibN fib = repl $
      recv fib (\(n, res) -> if n <= 2
        then send res 1 inert
        else new (\res' ->
          send fib (n-1, res') inert `par`
          send fib (n-2, res') inert `par`
          recv res' (\r1 ->
            recv res' (\r2 ->
              send res (r1 + r2) inert))))
  
  [pico|
    new finalRes. new fib.
    ( fib<25, finalRes>.0
    | finalRes(r). exec (print r). stop
    | ! fib(n, res). ( [n <= 2] res<1>.0 
                     | [n > 2] new res'. fib<n-1, res'>. fib<n-2, res'>. res'(r1). res'(r2). res<r1+r2>.0
                     )
    )
  |]
