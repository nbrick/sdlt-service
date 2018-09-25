{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib
  ( serveStampDutyCalculator
  ) where

import GHC.Generics
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Aeson
import Network.HTTP.Server
import Network.URL
import Network.Socket.Internal (SockAddr)

serveStampDutyCalculator :: IO ()
serveStampDutyCalculator = do server handle

handle :: SockAddr -> URL -> Request String -> IO (Response String)
handle _ url request =
  pure $ case url_path url of
    "api/stampDutyCalculator" -> routeStampDutyRequest request
    otherwise                 -> notFound "Not found!"

routeStampDutyRequest :: Request String -> Response String
routeStampDutyRequest request =
  case decode . pack $ rqBody request of
    Just SdltQuery { propertyValue = p } ->
      responseWith "application/json" . unpack . encode $
        SdltResponse { givenPropertyValue = p
                     , stampDutyAmount = calculateStampDuty p
                     }
    otherwise -> badRequest "Bad request. Try the following: ..." -- TODO

calculateStampDuty :: Int -> Int
calculateStampDuty p =
  p + 1 -- TODO

data SdltQuery = SdltQuery
  { propertyValue :: Int
  } deriving (Generic, FromJSON)

data SdltResponse = SdltResponse
  { givenPropertyValue :: Int
  , stampDutyAmount :: Int
  } deriving (Generic, ToJSON)

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

notFound message =
  Response { rspCode = (4,0,4)
           , rspBody = message
           , rspHeaders = headers message "text/plain"
           , rspReason = "Not found."
           }

badRequest message =
  Response { rspCode = (4,0,0)
           , rspBody = message
           , rspHeaders = headers message "text/plain"
           , rspReason = "Bad request."
           }
