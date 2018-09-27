{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib.Types
  ( HousePrice
  , mkHousePrice
  , fromHousePrice
  , TaxAmount (TaxAmount)
  ) where

import GHC.Generics
import Data.Aeson

newtype TaxAmount = TaxAmount Double deriving (Generic, ToJSON)

newtype HousePrice = HousePrice Double

mkHousePrice :: Double -> Either String HousePrice
mkHousePrice p =
  if p >= 0.0
    then Right $ HousePrice p
    else Left "Cannot have negative HousePrice."

fromHousePrice (HousePrice p) = p

instance FromJSON HousePrice where
  parseJSON v =
    case fromJSON v of
      Error e -> fail e
      Success d ->
        case mkHousePrice d of
          Left err -> fail err
          Right p -> pure p

instance ToJSON HousePrice where
  toJSON (HousePrice p) = toJSON p
