module PropertyTax
  ( calculateStampDuty
  ) where

import Data.Function ((&))
import PropertyTax.Types ( HousePrice, mkHousePrice, fromHousePrice
                         , TaxAmount (..)
                         )

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
