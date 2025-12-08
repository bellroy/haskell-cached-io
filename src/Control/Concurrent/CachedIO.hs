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
--   result again.
-- * @downloadData@ will not be called if a different thread is already calling
--   it. In that case, the stored value will be returned in the meantime.
module Control.Concurrent.CachedIO
  ( Cached (..),

    -- * IO
    cachedIO,
    cachedIOWith,
    cachedIO',
    cachedIOWith',

    -- * STM
    -- $stm
    cachedSTM,
    cachedSTMWith,
    cachedSTM',
    cachedSTMWith',
  )
where

import Control.Concurrent.STM
  ( STM,
    atomically,
    newTVar,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (join)
import Control.Monad.Catch (MonadCatch, onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

-- | A cached IO action in some monad @m@. Use 'runCached' to extract the action when you want to query it.
--
-- Note that using 'Control.Monad.join' when the cached action and the outer monad are the same will ignore caching.
newtype Cached m a = Cached {runCached :: m a}

data State a = Uninitialized | Initializing | Updating a | Fresh UTCTime a

-- | Cache an IO action, producing a version of this IO action that is cached
-- for 'interval' seconds. The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh, if the cache is older than 'interval'
-- seconds.
cachedIO ::
  (MonadIO m, MonadIO t, MonadCatch t) =>
  -- | Number of seconds before refreshing cache
  NominalDiffTime ->
  -- | IO action to cache
  t a ->
  m (Cached t a)
cachedIO = cachedIOWith . secondsPassed

-- | Cache an IO action, producing a version of this IO action that is cached
-- for 'interval' seconds. The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh, if the cache is older than 'interval'
-- seconds.
cachedIO' ::
  (MonadIO m, MonadIO t, MonadCatch t) =>
  -- | Number of seconds before refreshing cache
  NominalDiffTime ->
  -- | action to cache. The stale value and its refresh date
  -- are passed so that the action can perform external staleness checks
  (Maybe (UTCTime, a) -> t a) ->
  m (Cached t a)
cachedIO' = cachedIOWith' . secondsPassed

-- | Cache an IO action, The cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh
cachedIOWith ::
  (MonadIO m, MonadIO t, MonadCatch t) =>
  -- | Test function:
  -- If @isCacheStillFresh lastUpdated now@ returns 'True',
  -- the cache is considered still fresh and returns the cached IO action
  (UTCTime -> UTCTime -> Bool) ->
  -- | Action to cache.
  t a ->
  m (Cached t a)
cachedIOWith f = cachedIOWith' f . const

-- | Cache an IO action; the cache begins uninitialized.
--
-- The outer IO is responsible for setting up the cache. Use the inner one to
-- either get the cached value or refresh
cachedIOWith' ::
  (MonadIO m, MonadIO t, MonadCatch t) =>
  -- | Test function:
  -- If 'isCacheStillFresh' 'lastUpdated' 'now' returns 'True'
  -- the cache is considered still fresh and returns the cached IO action
  (UTCTime -> UTCTime -> Bool) ->
  -- | Action to cache. The stale value and its refresh date
  -- are passed so that the action can perform external staleness checks
  (Maybe (UTCTime, a) -> t a) ->
  m (Cached t a)
cachedIOWith' isCacheStillFresh refreshAction =
  liftIO . atomically $ cachedSTMWith' isCacheStillFresh refreshAction

-- $stm
--
-- The following actions are the exactly same as the 'cachedIO' versions, except
-- that they do not leave the 'STM' monad during construction.

-- | Set up a cached IO action in a transaction; producing a version of this IO
-- action that is cached for 'interval' seconds. The cache begins uninitialized.
cachedSTM ::
  (MonadIO m, MonadCatch m) =>
  -- | Number of seconds before refreshing cache
  NominalDiffTime ->
  -- | IO action to cache
  m a ->
  STM (Cached m a)
cachedSTM = cachedSTMWith . secondsPassed

-- | Set up a cached IO action in a transaction; producing a version of this IO
-- action that is cached for 'interval' seconds. The cache begins uninitialized.
cachedSTM' ::
  (MonadIO m, MonadCatch m) =>
  -- | Number of seconds before refreshing cache
  NominalDiffTime ->
  -- | action to cache. The stale value and its refresh date
  -- are passed so that the action can perform external staleness checks
  (Maybe (UTCTime, a) -> m a) ->
  STM (Cached m a)
cachedSTM' = cachedSTMWith' . secondsPassed

-- | Set up a cached IO action in a transaction; The cache begins uninitialized.
cachedSTMWith ::
  (MonadIO m, MonadCatch m) =>
  -- | Test function:
  -- If @isCacheStillFresh lastUpdated now@ returns 'True',
  -- the cache is considered still fresh and returns the cached IO action
  (UTCTime -> UTCTime -> Bool) ->
  -- | Action to cache.
  m a ->
  STM (Cached m a)
cachedSTMWith f = cachedSTMWith' f . const

-- | Set up a cached IO action in a transaction; the cache begins uninitialized.
cachedSTMWith' ::
  (MonadIO m, MonadCatch m) =>
  -- | Test function:
  -- If @'isCacheStillFresh' lastUpdated now@ returns 'True'
  -- the cache is considered still fresh and returns the cached IO action
  (UTCTime -> UTCTime -> Bool) ->
  -- | Action to cache. The stale value and its refresh date
  -- are passed so that the action can perform external staleness checks
  (Maybe (UTCTime, a) -> m a) ->
  STM (Cached m a)
cachedSTMWith' isCacheStillFresh refreshAction = do
  cachedT <- newTVar Uninitialized
  pure . Cached $ do
    now <- liftIO getCurrentTime
    join . liftIO . atomically $ do
      cached <- readTVar cachedT
      case cached of
        previousState@(Fresh lastUpdated value)
          -- There's data in the cache and it's recent. Just return.
          | isCacheStillFresh lastUpdated now -> pure (pure value)
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
        Uninitialized -> do
          writeTVar cachedT Initializing
          pure (refreshCache Uninitialized cachedT)
        -- The cache is uninitialized and another thread is already attempting to
        -- initialize it. Block.
        Initializing -> retry
  where
    refreshCache previousState cachedT = do
      let previous = case previousState of
            Fresh lastUpdated value -> Just (lastUpdated, value)
            _ -> Nothing
      newValue <-
        refreshAction previous
          `onException` liftIO (atomically (writeTVar cachedT previousState))
      liftIO $ do
        now <- getCurrentTime
        atomically (writeTVar cachedT (Fresh now newValue))
        pure newValue

-- | Check if @starting time@ + @seconds@ is after @end time@
secondsPassed ::
  -- | Seconds
  NominalDiffTime ->
  -- | Start time
  UTCTime ->
  -- | End time
  UTCTime ->
  Bool
secondsPassed interval start end = addUTCTime interval start > end
