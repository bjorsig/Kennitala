import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "show Kennitala" $ do
        property $ (\n -> dags (Kennitala abs(n) 12 95 20 1 9) <= 31) :: Int -> Bool
