import Control.Concurrent hiding (readChan, writeChan)
import Control.Concurrent.BoundedChan (BoundedChan,
                                       newBoundedChan, readChan, writeChan)

main :: IO ()
main = do
  bc <- newBoundedChan 10
  forkIO $ runWriter 1 bc 0
  forkIO $ runWriter 2 bc 0
  forkIO $ runWriter 3 bc 0
  forkIO $ runWriter 4 bc 0
  threadDelay (10^6)
  runReader bc

runWriter :: Int -> BoundedChan Int -> Int -> IO ()
runWriter me bc x = do
  putStrLn $ "Writing value " ++ show (me,x)
  writeChan bc x
  putStrLn $ "...Wrote value " ++ show (me,x)
  runWriter me bc (x + 1)

runReader :: BoundedChan Int -> IO ()
runReader bc = do
  next <- readChan bc
  putStrLn $ "Read value " ++ show next
  threadDelay (next+10^5)
  runReader bc
