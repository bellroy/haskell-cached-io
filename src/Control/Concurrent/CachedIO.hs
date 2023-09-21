-- | Example usage:
--
-- > -- Downloads a large payload from an external data store.
-- > downloadData :: IO ByteString
-- >
-- > cachedDownloadData :: IO (Cached IO ByteString)
-- > cachedDownloadData = cachedIO (secondsToNominalDiffTime 600) downloadData
--
-- The first time @cachedDownloadData@ is called, it calls @downloadData@,
-- stores the result, and returns it. If it is called again:
--
-- * before 10 minutes have passed, it returns the stored value.
-- * after 10 minutes have passed, it calls @downloadData@ and stores the
-- result again.
--
module Control.Concurrent.CachedIO (
    Cached(..),
    cachedIO,
    cachedIOWith,
    cachedIO',
    cachedIOWith'
    ) where

import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar, retry, TVar)
import Control.Monad (join)
import Control.Monad.Catch (MonadCatch, onException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime, UTCTime)

-- | A cached IO action in some monad @m@. Use 'runCached' to extract the action when you want to query it.
--
-- Note that using 'Control.Monad.join' when the cached action and the outer monad are the same will ignore caching.
newtype Cached m a = Cached {runCached :: m a}

data State a  = Uninitialized | Initializing | Updating a | Fresh UTCTime a

-- | Cache an IO action, producing a version of this IO action that is cached
-- for 'interval' seconds. The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh, if the cache is older than 'interval'
-- seconds.
cachedIO :: (MonadIO m, MonadIO t, MonadCatch t)
         => NominalDiffTime -- ^ Number of seconds before refreshing cache
         -> t a             -- ^ IO action to cache
         -> m (Cached t a)
cachedIO interval = cachedIOWith (secondsPassed interval)

-- | Cache an IO action, producing a version of this IO action that is cached
-- for 'interval' seconds. The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh, if the cache is older than 'interval'
-- seconds.
cachedIO' :: (MonadIO m, MonadIO t, MonadCatch t)
          => NominalDiffTime -- ^ Number of seconds before refreshing cache
          -> (Maybe (UTCTime, a) -> t a) -- ^ action to cache. The stale value and its refresh date
          -- are passed so that the action can perform external staleness checks
          -> m (Cached t a)
cachedIO' interval = cachedIOWith' (secondsPassed interval)

-- | Check if @starting time@ + @seconds@ is after @end time@
secondsPassed :: NominalDiffTime  -- ^ Seconds
               -> UTCTime         -- ^ Start time
               -> UTCTime         -- ^ End time
               -> Bool
secondsPassed interval start end = addUTCTime interval start > end

-- | Cache an IO action, The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh
cachedIOWith
    :: (MonadIO m, MonadIO t, MonadCatch t)
    => (UTCTime -> UTCTime -> Bool) -- ^ Test function:
    --   If 'isCacheStillFresh' 'lastUpdated' 'now' returns 'True'
    --   the cache is considered still fresh and returns the cached IO action
    -> t a -- ^ action to cache.
    -> m (Cached t a)
cachedIOWith f io = cachedIOWith' f (const io)

-- | Cache an IO action, The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh
cachedIOWith'
    :: (MonadIO m, MonadIO t, MonadCatch t)
    => (UTCTime -> UTCTime -> Bool) -- ^ Test function:
    --   If 'isCacheStillFresh' 'lastUpdated' 'now' returns 'True'
    --   the cache is considered still fresh and returns the cached IO action
    -> (Maybe (UTCTime, a) -> t a) -- ^ action to cache. The stale value and its refresh date
    -- are passed so that the action can perform external staleness checks
    -> m (Cached t a)
cachedIOWith' isCacheStillFresh io = do
  cachedT <- liftIO (atomically (newTVar Uninitialized))
  pure . Cached $ do
    now <- liftIO getCurrentTime
    join . liftIO . atomically $ do
      cached <- readTVar cachedT
      case cached of
        previousState@(Fresh lastUpdated value)
        -- There's data in the cache and it's recent. Just return.
          | isCacheStillFresh lastUpdated now -> return (return value)
        -- There's data in the cache, but it's stale. Update the cache state
        -- to prevent a second thread from also executing the action. The second
        -- thread will get the stale data instead.
          | otherwise -> do
            writeTVar cachedT (Updating value)
            pure (refreshCache previousState cachedT)
        -- Another thread is already updating the cache, just return the stale value
        Updating value -> pure (pure value)
        -- The cache is uninitialized. Mark the cache as initializing to block other
        -- threads. Initialize and return.
        Uninitialized -> pure (refreshCache Uninitialized cachedT)
        -- The cache is uninitialized and another thread is already attempting to
        -- initialize it. Block.
        Initializing -> retry
  where
    refreshCache previousState cachedT = do
      let previous = case previousState of
            Fresh lastUpdated value -> Just (lastUpdated, value)
            _                       -> Nothing
      newValue <- io previous `onException` restoreState previousState cachedT
      now <- liftIO getCurrentTime
      liftIO (atomically (writeTVar cachedT (Fresh now newValue)))
      liftIO (return newValue)

restoreState :: (MonadIO m) => State a -> TVar (State a) -> m ()
restoreState previousState cachedT = liftIO (atomically (writeTVar cachedT previousState))
