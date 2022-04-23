module Examples where

import Process
import Channel
import PiQQ
import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import Async


-- faculty
piFac :: Process Integer
piFac =
  [pico|
    new finalRes. new fac.
    ( fac<20, finalRes>.0
    | finalRes(r). exec (putStrLn $ "fac(20) = " ++ show r). stop r
    | ! fac(n, res). ( [n == 0] res<1>.0
                     | [n > 0] new res'. fac<n-1, res'>. res'(r). res<n*r>.0
                     )
    )
  |]

piFacRec :: Process Integer
piFacRec =
  new \finalRes ->
    rec 20 finalRes `par`
    recv finalRes \r -> exec (putStrLn $ "fac(20) = " ++ show r) (stop r)
  where
    rec :: Channel c => Integer -> c Integer -> Process Integer
    rec n res
      | n == 0 = send res 1 inert
      | otherwise = new \res' ->
          rec (n-1) res' `par`
          recv res' \r ->
            send res (n*r) inert

-- collatz
piCollatz :: Process Integer
piCollatz =
  [pico|
    new steps. new collatz.
    ( collatz<989345275647, steps>.0
    | steps(count). exec (putStrLn $ "steps: " ++ show count). stop count
    | ! collatz(n, stepCount). ( [n == 1] stepCount<0>.0
                               | [n /= 1] new sc. ( [even n] collatz<n `div` 2, sc>.0
                                                  | [odd n] collatz<3 * n + 1, sc>.0
                                                  | sc(c). stepCount<c+1>.0
                                                  )
                               )
    )
  |]

piCollatzRec :: Process Integer
piCollatzRec =
  new \steps ->
    rec 989345275647 steps `par`
    recv steps \count -> exec (putStrLn $ "steps: " ++ show count) (stop count)
  where
    rec :: Channel c => Integer -> c Integer -> Process Integer
    rec n stepCount
      | n == 1 = send stepCount 0 inert
      | otherwise = new \sc ->
          (if even n then rec (n `div` 2) sc else rec (3*n+1) sc) `par`
          recv sc \c -> send stepCount (c+1) inert

-- hanoi
piHanoi :: Process Integer
piHanoi =
  [pico|
    new steps. new hanoi.
    ( hanoi<16, steps>.0
    | steps(count). exec (putStrLn $ "hanoi(16) = " ++ show count). (stop count)
    | ! hanoi(n, stepCount). ( [n == 1] stepCount<1>.0
                             | [n > 1] new sc. hanoi<n-1, sc>. hanoi<n-1, sc>. sc(c1). sc(c2). stepCount<c1+c2+1>.0
                             )
    )
  |]

piHanoiRec :: Process Integer
piHanoiRec =
  new \steps ->
    rec 16 steps `par`
    recv steps \count -> exec (putStrLn $ "hanoi(16) = " ++ show count) (stop count)
  where
    rec :: Channel c => Int -> c Integer -> Process Integer
    rec n stepCount
      | n == 1 = send stepCount 1 inert
      | otherwise = new \sc ->
          rec (n-1) sc `par`
          rec (n-1) sc `par`
          recv sc \c1 ->
            recv sc \c2 ->
              send stepCount (c1+c2+1) inert

-- fibonacci
piFib :: Process Integer
piFib = 
  [pico|
    new finalRes. new fib.
    ( fib<15, finalRes>.0
    | finalRes(r). exec (putStrLn $ "fib(15) = " ++ show r). (stop r)
    | ! fib(n, res). exec (print n). ( [n == 0] res<0>.0
                     | [n == 1] res<1>.0 
                     | [n > 1] new res'. fib<n-1, res'>. fib<n-2, res'>. res'(r1). res'(r2). res<r1+r2>.0
                     )
    )
  |]

piFibRec :: Process Integer
piFibRec =
  new \finalRes ->
    rec 15 finalRes `par`
    recv finalRes \r -> exec (putStrLn $ "fib(15) = " ++ show r) (stop r)
  where
    rec :: Channel c => Integer -> c Integer -> Process Integer
    rec n res
      | n == 0 = send res 0 inert
      | n == 1 = send res 1 inert
      | otherwise = new \res' ->
          rec (n-1) res' `par`
          rec (n-2) res' `par`
          recv res' \r1 ->
            recv res' \r2 ->
              send res (r1+r2) inert

-- quicksort
partition :: Ord a => (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition pred (x:xs)
  | pred x = (x:r1, r2)
  | otherwise = (r1, x:r2)
  where
    (r1, r2) = partition pred xs

piQuicksort :: [Int] -> Process [Int]
piQuicksort list =
  new \sortedList -> new \quicksort ->
    (send quicksort (list, sortedList) inert) `par`
    (recv sortedList \l -> exec (putStrLn $ "sorted list: " ++ show l) (stop l)) `par`
    (repl $ recv quicksort \(l, res) -> case l of
      [] -> send res [] inert
      [x] -> send res [x] inert
      (x:xs) ->
        new \res1 -> new \res2 ->
          let (less, greater) = partition (<x) xs
          in send quicksort (less, res1) $ 
              send quicksort (greater, res2) $
                recv res1 \l1 ->
                  recv res2 \l2 ->
                    send res (l1 ++ (x:l2)) inert
    )

piQuicksortRec :: [Int] -> Process [Int]
piQuicksortRec list =
  new \sortedList -> new \quicksort ->
    rec list sortedList `par`
    recv sortedList \l ->  exec (putStrLn $ "sorted list: " ++ show l) (stop l)
  where
    rec :: (Ord a, Channel c) => [a] -> c [a] -> Process [a]
    rec l res = case l of
      [] -> send res [] inert
      [x] -> send res [x] inert
      x:xs ->
        new \res1 -> new \res2 ->
          let (less, greater) = partition (<x) xs
          in
            rec less res1 `par`
            rec greater res2 `par`
            recv res1 \l1 ->
              recv res2 \l2 ->
                send res (l1 ++ (x:l2)) inert
