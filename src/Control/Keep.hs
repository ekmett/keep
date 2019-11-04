module Control.Keep
( Merkle, merkle, unmerkle
, Keep, runKeep
, closed
, checkpoint
, verify
, unsafeRedis
) where

import Control.Keep.Internal
