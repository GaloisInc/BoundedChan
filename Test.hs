import Control.Concurrent hiding (readChan, writeChan)
import Control.Concurrent.BoundedChan
import Control.Monad

main = do
  bc <- newBoundedChan 1
  forkIO $ runWriter bc 0
  forkIO $ runWriter bc 0
  forkIO $ runWriter bc 0
  forkIO $ runWriter bc 0
  runReader bc

runWriter :: BoundedChan Int -> Int -> IO ()
runWriter bc x = do
  writeChan bc x 
  runWriter bc (x + 1)

runReader :: BoundedChan Int -> IO ()
runReader bc = do
  next <- readChan bc
  putStrLn $ "Read value " ++ show next
  runReader bc
