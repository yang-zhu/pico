module Channel where

import Process (Process)

class Channel c where
  send :: c a -> a -> Process -> Process
  recv :: c a -> (a -> Process) -> Process

class Channel c => ExtendedChannel c where
  choose :: c a -> (a -> Process) -> c b -> (b -> Process) -> Process