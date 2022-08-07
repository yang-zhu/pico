module Examples where

import Data.Maybe (isJust, fromJust)
import Process
import Channel
import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import AsyncMVar
-- import AsyncSTM

-- factorial
piFac :: Integer -> Process Integer
piFac n =
  new \finalRes ->
    new \fac ->
      send fac (n, finalRes) inert `par`
      recv finalRes stop `par`
      repl (recv fac \(n, res) ->
        if n == 0
          then send res 1 inert
          else new \res' ->
            send fac (n - 1, res') $ recv res' \r -> send res (n * r) inert)

piFacRec :: Integer -> Process Integer
piFacRec n =
  new \finalRes ->
    rec n finalRes `par`
    recv finalRes stop
  where
    rec :: Channel c => Integer -> c Integer -> Process Integer
    rec n res
      | n == 0 = send res 1 inert
      | otherwise = new \res' ->
          rec (n-1) res' `par`
          recv res' \r ->
            send res (n * r) inert

-- collatz
piCollatz :: Integer -> Process Integer
piCollatz n =
  new \steps ->
    new \collatz ->
      send collatz (n, steps) inert `par`
      recv steps stop `par`
      repl (recv collatz \(n, stepCount) ->
        if n== 1
          then send stepCount 0 inert
          else new \sc ->
            (if even n
              then send collatz (n `div` 2, sc) inert
              else send collatz (3 * n + 1, sc) inert) `par`
            recv sc (\c -> send stepCount (c + 1) inert))

piCollatzRec :: Integer -> Process Integer
piCollatzRec n =
  new \steps ->
    rec n steps `par`
    recv steps stop
  where
    rec :: Channel c => Integer -> c Integer -> Process Integer
    rec n stepCount
      | n == 1 = send stepCount 0 inert
      | otherwise = new \sc ->
          (if even n then rec (n `div` 2) sc else rec (3*n+1) sc) `par`
          recv sc \c -> send stepCount (c+1) inert

-- Hanoi
piHanoi :: Integer -> Process Integer
piHanoi n =
  new \steps ->
    new \hanoi ->
      send hanoi (n, steps) inert `par`
      recv steps stop `par`
      repl (recv hanoi \(n, stepCount) ->
        if n == 1
          then send stepCount 1 inert
          else new \sc ->
            send hanoi (n - 1, sc) (send hanoi (n -1, sc) (recv sc (\c1 ->
              recv sc (\c2 -> send stepCount (c1 + c2 + 1) inert)))))

piHanoiRec :: Int -> Process Integer
piHanoiRec n =
  new \steps ->
    rec n steps `par`
    recv steps stop
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
piFib :: Integer -> Process Integer
piFib n =
  new \finalRes ->
    new \fib ->
      send fib (n, finalRes) inert `par`
      recv finalRes stop `par`
      repl (recv fib \(n, res) -> case n of
        0 -> send res 0 inert
        1 -> send res 1 inert
        _ -> new \res' ->
          send fib (n - 1, res')
            (send fib (n - 2, res') $
              recv res' \r1 ->
                recv res' \r2 ->
                  send res (r1 + r2) inert))

piFibRec :: Integer -> Process Integer
piFibRec n =
  new \finalRes ->
    rec n finalRes `par`
    recv finalRes stop
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

-- transmit lists on pi-calculus channels
sendList :: Channel c => c (Maybe a) -> [a] -> Process b
sendList chan = foldr (send chan . Just) (send chan Nothing inert)

sendListRepl :: Channel c => c (Maybe a) -> [a] -> Process b
sendListRepl chan l =
  new \go ->
    send go l inert `par`
    repl (recv go \case
      [] -> send chan Nothing inert
      x:xs -> send chan (Just x) (send go xs inert))

recvList :: Channel c => c (Maybe a) -> ([a] -> Process b) -> Process b
recvList chan f = recv chan (\x -> if isJust x then recvList chan (\xs -> f (fromJust x : xs)) else f [])

recvListRepl :: Channel c => c (Maybe a) -> ([a] -> Process b) -> Process b
recvListRepl chan f =
  new \go ->
    send go f inert `par`
    repl (recv go (\f -> recv chan \case
      Just x -> send go (\xs -> f (x : xs)) inert
      Nothing -> f []))

transmitList :: [a] -> Process [a]
transmitList l = new \chan -> sendList chan l `par` recvList chan stop

sendNOnes :: Channel c => Int -> c Int -> Process a
sendNOnes 0 _ = inert
sendNOnes n chan = send chan 1 (sendNOnes (n - 1) chan)

recvNOnes :: Channel c => Int -> c Int ->([Int] -> Process b) -> Process b
recvNOnes 0 _ f = f []
recvNOnes n chan f = recv chan (\x -> recvNOnes (n - 1) chan (\xs -> f (x : xs)))

transmitNOnes :: Int -> Process [Int]
transmitNOnes n = new \chan -> sendNOnes n chan `par` recvNOnes n chan stop 

-- quicksort
piPartition :: (Channel c, Ord a) => a -> c (Maybe a) -> c (Maybe a) -> c (Maybe a) -> Process b
piPartition pivot input less greater = recv input \case
  Just x -> send (if x < pivot then less else greater) (Just x) (piPartition pivot input less greater)
  Nothing -> send less Nothing $ send greater Nothing inert

-- piQuicksort :: Ord a => SyncChannel (Maybe a) -> SyncChannel (Maybe a) -> Process b
piQuicksort input output =
  new \less ->
    new \greater ->
      new \lessOutput ->
        new \greaterOutput ->
          recv input (\x ->
            if isJust x
              then piPartition (fromJust x) input less greater `par`
                  piQuicksort less lessOutput `par`
                  piQuicksort greater greaterOutput `par`
                  (forwardToOutput lessOutput $ send output x $ forwardToOutput greaterOutput $ send output Nothing inert)
              else send output Nothing inert)
  where
    forwardToOutput chan p = recv chan (\x ->
      if isJust x
        then send output x (forwardToOutput chan p)
        else p)

quicksortList :: Ord a => [a] -> Process [a]
quicksortList l =
  new \inputChan ->
    new \outputChan ->
      sendList inputChan l `par`
      piQuicksort inputChan outputChan `par`
      recvList outputChan stop