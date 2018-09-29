import Test.Hspec
import Test.QuickCheck
import Data.Either.Unwrap
import Data.Function ((&))
import Lib (calculateStampDuty)
import Lib.Types

main :: IO ()
main = hspec $ do
  describe "Lib.calculateStampDuty" $ do
    it "monotonically increases" $
      forAll (choose (0.0, 2.0e6)) $ \x ->
        stampDuty x <= stampDuty (x + 10.0)

    it "does not grow faster than 13%" $
      forAll (choose (0.0, 2.0e6)) $ \x ->
        let w = 100.0 in ((stampDuty (x + w)) - (stampDuty x)) < 0.13 * w

    it "satisfies five known test cases" $
      [ (  100000.0,      0.0 )
      , (  200000.0,   1500.0 )
      , (  500000.0,  15000.0 )
      , ( 1000000.0,  43750.0 )
      , ( 2000000.0, 153750.0 )
      ] `shouldSatisfy` and . map (\(p, t) -> abs (t - stampDuty p) < 1.0e-6)

stampDuty :: Double -> Double -- To reduce boilerplate.
stampDuty propertyValue =
  t where (TaxAmount t) =
            calculateStampDuty . fromRight $ mkHousePrice propertyValue
