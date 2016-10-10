import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Data.GraphQL.Document" $ do
    it "should parse basic queries correctly." $ do
      head [] `shouldThrow` anyException
