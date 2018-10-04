module Logging where

import Prelude hiding (log)
import Data.Time.Clock
import Pipes

type Logged = Producer String

log :: Monad m => String -> Logged m ()
log = yield

logToStdOutWithTimestamps :: Logged IO (a) -> IO (a)
logToStdOutWithTimestamps logProducer =
  runEffect $ for logProducer (lift . putStrLnWithTime)

putStrLnWithTime s = do
  time <- getCurrentTime
  putStrLn $ (show time) ++ " : " ++ s
