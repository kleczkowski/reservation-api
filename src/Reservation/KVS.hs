{-# LANGUAGE TemplateHaskell #-}
module Reservation.KVS where

import Prelude hiding (State)

import Polysemy
import Polysemy.State (State)
import qualified Polysemy.State as State
import qualified Relude.Extra.Map as Map
import Database.Redis (Redis)
import qualified Database.Redis as Redis
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Control.Exception

-- | An effect that serves key-value store.
data KVS k v m a where
  -- | Get an element of key-value store by key if exists.
  KvsLookup :: k -> KVS k v m (Maybe v)
  -- | Insert new element to key-value store.
  KvsInsert :: k -> v -> KVS k v m ()

makeSem ''KVS

newtype RedisException = RedisException Redis.Reply
  deriving (Show)

instance Exception RedisException

-- | Run key-value store effect purely, transforming strict ordered map.
runKvsAsMap :: Ord k => Sem (KVS k v ': r) a -> Sem (State (Map k v) ': r) a
runKvsAsMap = reinterpret $ \case
  KvsLookup k -> State.gets (Map.lookup k)
  KvsInsert k v -> State.modify' (Map.insert k v)

-- | Runs key-value store as an instance of Redis client.
runKvsAsRedis :: (Members '[Embed Redis] r, Binary k, Binary v) => Sem (KVS k v ': r) a -> Sem r a
runKvsAsRedis = interpret $ \case
  KvsLookup k -> do
    let kbs = toStrict (Binary.encode k)
        bsv bs = Binary.decode (toLazy bs)
    eitherV <- embed (Redis.get kbs)
    case eitherV of
      Left e -> throw (RedisException e)
      Right mvbs -> pure (bsv <$> mvbs)
  KvsInsert k v -> do
    let kbs = toStrict (Binary.encode k)
        vbs = toStrict (Binary.encode v)
    whenLeftM_ (embed (Redis.set kbs vbs)) (throw . RedisException)