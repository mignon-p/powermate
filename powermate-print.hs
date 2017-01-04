import Control.Monad

import PowerMate

printEvents :: Maybe FilePath -> IO ()
printEvents Nothing = putStrLn "No PowerMate found!"
printEvents (Just dev) = do
  h <- openDevice dev
  forever $ do
    e <- readEvent h
    putStrLn $ show e

main = searchForDevice >>= printEvents
