import Test.Hspec
import Api.Internal
import Database.Internal
import Model

main :: IO ()
main = hspec $ do

    -- Api 
    describe "haversine" $ do
        it "calculates the haversine distance between two coordinates" $
            haversine (38.898556,-77.037852) (38.897147,-77.043934) `shouldBe` 0.5491557912038084

    describe "getSorted" $ do
        it "should return a list of tuples sorted by the first value in ascending order" $
            getSorted [(3.0,"c"),(1.0,"a"),(2.0,"b")] `shouldBe` ["a","b","c"]

    -- Database
    describe "parseStations" $ do
        it "returns 72 parsed csv lines" $ do
            stations <- parseStations
            length stations `shouldBe` 72

    describe "parseBridges" $ do
        it "returns 6544 parsed csv lines" $ do
            bridges <- parseBridges
            length bridges `shouldBe` 6544

    describe "extractValues" $ do
        it "should return a list of values taken out of the Maybe context" $
            extractValues [Just 1, Just 2, Nothing, Just 3, Nothing] `shouldBe` [1,2,3]

    describe "csvToStation" $ do
        it "should take a CSV entry and return a Station" $
            (csvToStation ["0","0","0","0","0","0","0","0","0","0","0"]) 
            `shouldBe` (Just $ Station 0 "0" 0.0 0.0 "0" "0" "0" "0" "0" "0" "0")

    describe "csvToBridge" $ do
        it "should take a CSV entry and return a Bridge" $
            (csvToBridge ["0","0","0","0","0","0","0","0","0","0"]) 
            `shouldBe` (Just $ Bridge 0 "0" "0" "0" "0" 0.0 "0" "0" 0.0 (-0.0))

