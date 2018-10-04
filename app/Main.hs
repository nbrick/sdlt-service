module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Network.HTTP.Server
import Network.URL
import Network.Socket.Internal (SockAddr)
import PropertyTax
import JSONSupport
import Response
import Prelude hiding (log)
import Logging

main :: IO ()
main = do server handle

handle :: SockAddr -> URL -> Request String -> IO (Response String)
handle _ url request = logToStdOutWithTimestamps $
  case url_path url of
    "api/stampDutyCalculator" -> do
      log "The client requested api/stampDutyCollector."
      routeStampDutyRequest request
    otherwise -> do
      log "The client requested a bad path. We will return a 404."
      pure $ notFound "Not found!"

routeStampDutyRequest :: Monad m
                      => Request String
                      -> Logged m (Response String)
routeStampDutyRequest request =
  case eitherDecode . pack $ rqBody request of
    Right SDLTQuery { propertyValue = p'@(HousePrice' p) } -> do
      log "The client gave a valid SDLT query. We will return the stamp duty."
      pure $ responseWith "application/json" . unpack . encode $
        SDLTResponse { givenPropertyValue = p'
                     , stampDutyAmount = TaxAmount' $ calculateStampDuty p
                     }
    Left err -> do
      log "The client gave a bad request. We will return a 400."
      pure $ badRequest $ "Error deserializing JSON: " ++ err
