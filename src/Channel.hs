module Channel where

import Process (Process)

class Channel c where
  send :: c a -> a -> Process b -> Process b
  recv :: c a -> (a -> Process b) -> Process b