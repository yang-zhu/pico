module Main where

import System.Environment (getArgs)
import System.Random (randomIO)
import System.Clock (getTime, diffTimeSpec, toNanoSecs, Clock (Monotonic))
import Process
import Sum
import Channel
-- import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import AsyncMVar
-- import AsyncSTM
-- import PrivateMVar
-- import GlobalMVar
-- import PrivateTMVar
-- import GlobalTMVar
-- import AsyncMVar
-- import AsyncSTM
import Examples (transmitList, transmitNElems, transmitNElems', transmitMultiRecv, quicksortList)
-- import ExampleCML


main :: IO ()
main = do
  [listLen] <- getArgs
  let len = read listLen
  -- l <- generateList len
  before <- getTime Monotonic
  runProcess $
    transmitList [1..len]
    -- transmitNElems len
    -- transmitNElems' len
    -- transmitMultiRecv [1..len]
    -- quicksortList l
  after <- getTime Monotonic
  print $ fromIntegral (toNanoSecs (diffTimeSpec before after)) / 1000000000

  where
    generateList :: Int -> IO [Int]
    generateList 0 = return []
    generateList i = (:) <$> randomIO <*> generateList (i - 1)


  -- runProcessRandom $ new \chan ->
  --   send chan sent (exec_ (putStrLn ("sent: " ++ show sent)) inert) `par` 
  --   choose
  --     (recv chan (\received -> exec_ (putStrLn ("received + 1: " ++ show (received + 1))) (stop ())))
  --     (recv chan (\received -> exec_ (putStrLn ("received + 2: " ++ show (received + 2))) (stop ())))
  -- where
  --   sent = 1