module PropertyTax.Types
  ( HousePrice
  , mkHousePrice
  , fromHousePrice
  , TaxAmount (TaxAmount)
  ) where

newtype TaxAmount = TaxAmount Double

newtype HousePrice = HousePrice Double

mkHousePrice :: Double -> Either String HousePrice
mkHousePrice p =
  if p >= 0.0
    then Right $ HousePrice p
    else Left "Cannot have negative HousePrice."

fromHousePrice (HousePrice p) = p
