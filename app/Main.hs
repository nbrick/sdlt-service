module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Network.HTTP.Server
import Network.URL
import Network.Socket.Internal (SockAddr)
import PropertyTax
import JSONSupport
import Response

main :: IO ()
main = do server handle

handle :: SockAddr -> URL -> Request String -> IO (Response String)
handle _ url request =
  pure $ case url_path url of
    "api/stampDutyCalculator" -> routeStampDutyRequest request
    otherwise                 -> notFound "Not found!"

routeStampDutyRequest :: Request String -> Response String
routeStampDutyRequest request =
  case eitherDecode . pack $ rqBody request of
    Right SDLTQuery { propertyValue = p'@(HousePrice' p) } ->
      responseWith "application/json" . unpack . encode $
        SDLTResponse { givenPropertyValue = p'
                     , stampDutyAmount = TaxAmount' $ calculateStampDuty p
                     }
    Left err -> badRequest err
