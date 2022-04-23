module Channel where

import Process (Process)

class Channel c where
  send :: c a -> a -> Process b -> Process b
  recv :: c a -> (a -> Process b) -> Process b

class Channel c => ExtendedChannel c where
  choose :: c a -> (a -> Process d) -> c b -> (b -> Process d) -> Process d