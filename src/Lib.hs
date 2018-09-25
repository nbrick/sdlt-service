module Lib
  ( serveStampDutyCalculator
  ) where

import Network.HTTP.Server
import Network.URL
import Network.Socket.Internal (SockAddr)

headers message contentType =
  [ Header HdrContentLength (show $ length message)
  , Header HdrContentType $ contentType ++ "; charset=utf-8"
  ]

responseWith contentType body =
  Response { rspCode = (2,0,0)
           , rspBody = body
           , rspHeaders = headers body contentType
           , rspReason = "OK"
           }

handle :: SockAddr -> URL -> Request String -> IO (Response String)
handle _ _ _ = pure $ responseWith "text/plain" "Hello!\n"

serveStampDutyCalculator :: IO ()
serveStampDutyCalculator = do
  server handle
