-- |Implements bounded channels. These channels differ from normal 'Chan's in
-- that they are guaranteed to contain no more than a certain number of
-- elements. This is ideal when you may be writing to a channel faster than you
-- are able to read from it.
--
-- This module supports all the functions of "Control.Concurrent.Chan" except
-- 'unGetChan' and 'dupChan', which are not supported for bounded channels.
module Control.Concurrent.BoundedChan(
         BoundedChan
       , newBoundedChan
       , writeChan
       , readChan
       , isEmptyChan
       , getChanContents
       , writeList2Chan
       )
  where

import Control.Concurrent.MVar
import Control.Monad(replicateM)
import Data.Array
import System.IO.Unsafe(unsafeInterleaveIO)

-- |'BoundedChan' is an abstract data type representing a bounded channel.
data BoundedChan a = BC {
       _size     :: Int
     , _contents :: Array Int (MVar a)
     , _writePos :: MVar Int
     , _readPos  :: MVar Int
     }
-- LOCK ORDERING: A -> B -> C

-- |@newBoundedChan n@ returns a channel than can contain no more than @n@
-- elements.
newBoundedChan :: Int -> IO (BoundedChan a)
newBoundedChan x = do
  entls   <- replicateM x newEmptyMVar
  wpos    <- newMVar 0
  rpos    <- newMVar 0
  let entries = listArray (0, x - 1) entls
  return (BC x entries wpos rpos)

-- |Write an element to the channel. If the channel is full, this routine will
-- block until it is able to write. If you have multiple writers, be careful
-- here, because the unlocking is not guaranteed to avoid starvation.
writeChan :: BoundedChan a -> a -> IO ()
writeChan (BC size contents wposMV _) x = do
  wpos <- takeMVar wposMV
  putMVar wposMV $! (wpos + 1) `mod` size
  putMVar (contents ! wpos) x

-- |Read an element from the channel. If the channel is empty, this routine
-- will block until it is able to read.
readChan :: BoundedChan a -> IO a
readChan (BC size contents _ rposMV) = do
  rpos <- takeMVar rposMV
  putMVar rposMV $! (rpos + 1) `mod` size
  takeMVar (contents ! rpos)

-- |Returns 'True' if the supplied channel is empty.
isEmptyChan :: BoundedChan a -> IO Bool
isEmptyChan (BC _ contents _ rposMV) = do
  rpos <- takeMVar rposMV
  res <- isEmptyMVar (contents ! rpos)
  putMVar rposMV rpos
  return res

-- |Return a lazy list representing the contents of the supplied channel.
getChanContents :: BoundedChan a -> IO [a]
getChanContents ch = unsafeInterleaveIO $ do
                       x  <- readChan ch
                       xs <- getChanContents ch
                       return (x:xs)

-- |Write a list of elements to the channel. If the channel becomes full, this
-- routine will block until it is able to write.
writeList2Chan :: BoundedChan a -> [a] -> IO ()
writeList2Chan ch ls = mapM_ (writeChan ch) ls
