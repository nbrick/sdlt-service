{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module JSONSupport where

import GHC.Generics
import Data.Aeson
import PropertyTax.Types ( HousePrice, mkHousePrice, fromHousePrice
                         , TaxAmount (..)
                         )

data SDLTQuery = SDLTQuery
  { propertyValue :: HousePrice'
  } deriving (Generic, FromJSON)

data SDLTResponse = SDLTResponse
  { givenPropertyValue :: HousePrice'
  , stampDutyAmount :: TaxAmount'
  } deriving (Generic, ToJSON)

newtype TaxAmount' = TaxAmount' TaxAmount

instance ToJSON TaxAmount' where
  toJSON (TaxAmount' (TaxAmount t)) = toJSON t

newtype HousePrice' = HousePrice' HousePrice

instance FromJSON HousePrice' where
  parseJSON v =
    case fromJSON v of
      Error e -> fail e
      Success d ->
        case mkHousePrice d of
          Left err -> fail err
          Right p -> pure $ HousePrice' p

instance ToJSON HousePrice' where
  toJSON (HousePrice' p) = toJSON $ fromHousePrice p
