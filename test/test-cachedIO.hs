module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.CachedIO (Cached (..), cachedIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Functor (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Control.Concurrent.CachedIO"
    [ testCase "Action is not called until first use" $ do
        ref <- newIORef 0
        Cached {} <- cachedIO (5 * 60) $ increment ref
        count <- readIORef ref
        count @?= 0,
      testCase "Action is not called if value is fresh" $ do
        ref <- newIORef 0
        Cached action <- cachedIO (5 * 60) $ increment ref
        void action
        void action
        count <- readIORef ref
        count @?= 1,
      testCase "Action is not called if cache is initializing" $ do
        ref <- newIORef 0
        Cached action <- cachedIO (5 * 60) $ incrementSlow ref
        void . forkIO $ void action
        void action
        count <- readIORef ref
        count @?= 1,
      testCase "Cache resets when ExceptT action fails via throwError" $ do
        -- This hangs indefinitely if we do not handle monadic exceptions.
        ref <- newIORef 0
        Cached action <- cachedIO (5 * 60) $ ExceptT (pure <$> increment ref) <* throwE ()
        result1 <- runExceptT action
        result1 @?= Left ()
        result2 <- runExceptT action
        result2 @?= Left ()
        count <- readIORef ref
        count @?= 2
    ]

increment :: IORef Int -> IO Int
increment ref = atomicModifyIORef' ref (\i -> (succ i, i))

incrementSlow :: IORef Int -> IO Int
incrementSlow ref = do
  res <- atomicModifyIORef' ref (\i -> (succ i, i))
  -- waiting AFTER the increase will show that Initialized is necessary
  -- because forking a thread takes enough time that the "action >> readIORef"
  -- are done before the forked thread has the chance to increment the counter.
  threadDelay $ 100 * 1000 -- 100 ms
  pure res

main :: IO ()
main = defaultMain tests
