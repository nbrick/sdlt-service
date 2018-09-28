{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib
  ( serveStampDutyCalculator
  ) where

import GHC.Generics
import Data.Function ((&))
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Aeson
import Network.HTTP.Server
import Network.URL
import Network.Socket.Internal (SockAddr)
import Lib.Types

serveStampDutyCalculator :: IO ()
serveStampDutyCalculator = do server handle

handle :: SockAddr -> URL -> Request String -> IO (Response String)
handle _ url request =
  pure $ case url_path url of
    "api/stampDutyCalculator" -> routeStampDutyRequest request
    otherwise                 -> notFound "Not found!"

routeStampDutyRequest :: Request String -> Response String
routeStampDutyRequest request =
  case eitherDecode . pack $ rqBody request of
    Right SdltQuery { propertyValue = p } ->
      responseWith "application/json" . unpack . encode $
        SdltResponse { givenPropertyValue = p
                     , stampDutyAmount = calculateStampDuty p
                     }
    Left err -> badRequest err

calculateStampDuty :: HousePrice -> TaxAmount
calculateStampDuty housePrice =
  sum [ toPairs rateBounds
          & map (\((l, r), (u, _)) -> r * min (max 0.0 (p - l)) (u - l))
          & sum
      , last rateBounds & \(l, r) -> r * (max 0.0 (p - l))
      ]
    & TaxAmount
  where p = fromHousePrice housePrice

rateBounds =
  [ (       0.0, 0.0  )
  , (  125000.0, 0.02 )
  , (  250000.0, 0.05 )
  , (  925000.0, 0.1  )
  , ( 1500000.0, 0.12 )
  ]

toPairs :: [a] -> [(a, a)]
toPairs l = (zip <*> tail) l

data SdltQuery = SdltQuery
  { propertyValue :: HousePrice
  } deriving (Generic, FromJSON)

data SdltResponse = SdltResponse
  { givenPropertyValue :: HousePrice
  , stampDutyAmount :: TaxAmount
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
