module Control.Concurrent.CachedIO (
    cachedIO,
    cachedIOWith
    ) where

import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar, retry)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime, UTCTime)

data State = Uninitialized | Initializing

-- | Cache an IO action, producing a version of this IO action that is cached
-- for 'interval' seconds. The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh, if the cache is older than 'interval'
-- seconds.
cachedIO :: (MonadIO m)
         => NominalDiffTime -- ^ Number of seconds before refreshing cache
         -> m a             -- ^ IO action to cache
         -> m (m a)
cachedIO interval = cachedIOWith (secondsPassed interval)

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
cachedIOWith :: (MonadIO m)
    => (UTCTime -> UTCTime -> Bool) -- ^ Test function:
    --   If 'isCacheStillFresh' 'lastUpdated' 'now' returns 'True'
    --   the cache is considered still fresh and returns the cached IO action
    -> m a                          -- ^ action to cache
    -> m (m a)
cachedIOWith isCacheStillFresh io = do
  initTime <- liftIO getCurrentTime
  cachedT <- liftIO (atomically (newTVar (initTime, Left Uninitialized)))
  return $ do
    now <- liftIO getCurrentTime
    join . liftIO . atomically $ do
      cached <- readTVar cachedT
      case cached of
        -- There's data in the cache and it's recent. Just return.
        (lastUpdated, Right value) | isCacheStillFresh lastUpdated now ->
          return (return value)
        -- There's data in the cache, but it's stale. Update the cache timestamp
        -- to prevent a second thread from also executing the IO action. The second
        -- thread would get the stale data instead. Refresh the cache and return.
        (_, Right value) -> do
          writeTVar cachedT (now, Right value)
          return $ refreshCache now cachedT
        -- The cache is uninitialized. Mark the cache as initializing to block other
        -- threads. Initialize and return.
        (_, Left Uninitialized) -> do
          writeTVar cachedT (now, Left Initializing)
          return $ refreshCache now cachedT
        -- The cache is uninitialized and another thread is already attempting to
        -- initialize it. Block.
        (_, Left Initializing) -> retry
  where
    refreshCache now cachedT = do
      newValue <- io
      liftIO (atomically (writeTVar cachedT (now, Right newValue)))
      liftIO (return newValue)
