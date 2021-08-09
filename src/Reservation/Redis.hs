module Reservation.Redis
  ( runRedis
  , obtainRedisConnection
  ) where

import Database.Redis (Redis)
import qualified Database.Redis as Redis
import Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input

-- | Runs embedded @Redis@ monad using @Connection@ as an input.
runRedis :: Members '[Embed IO, Input Redis.Connection] r => Sem (Embed Redis ': r) a -> Sem r a
runRedis = interpret $ \case
  Embed redis -> do
    conn <- Input.input
    embed (Redis.runRedis conn redis)

-- | Creates checked connection to the Redis database.
obtainRedisConnection :: Members '[Embed IO] r => Redis.ConnectInfo -> Sem (Input Redis.Connection ': r) a -> Sem r a
obtainRedisConnection connInfo = Input.runInputSem $ do
  embed (Redis.checkedConnect connInfo)