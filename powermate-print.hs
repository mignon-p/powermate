import Control.Monad

import PowerMate

printEvent :: Maybe Event -> IO ()
printEvent Nothing = return ()
printEvent (Just e) = putStrLn $ show e

printEvents :: Maybe FilePath -> IO ()
printEvents Nothing = putStrLn "No PowerMate found!"
printEvents (Just dev) = do
  h <- openDevice dev
  forever $ do
    e <- readEvent h
    printEvent e

main = searchForDevice >>= printEvents
