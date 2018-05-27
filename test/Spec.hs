import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "NMEA0183" $ do
        it "FIXME does something 1" $ do
            2 + 2 `shouldBe` 4
        it "FIXME does something 2" $ do
            property $ \x xs -> head (x:xs) == (x :: Int)
