module Channel where

import Process (Process)

-- | 'Channel' represents pi-calculus channels that could be either synchronous or asynchronous.
class Channel c where
  -- | After the given message is successfully sent via the given channel, the succeeding process can execute.
  -- For synchronous channels, the succeeding process will only be executed after the message has been received.
  -- For asynchronous channels, the succeding process does not need to wait for the message to be received.
  send :: c a -> a -> Process b -> Process b

  -- | After a message is received via the given channel, the succeeding process can execute.
  recv :: c a -> (a -> Process b) -> Process b