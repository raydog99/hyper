import Test.Hspec
import SCombinator

main :: IO ()
main = hspec $ do
  describe "S combinator" $ do
    it "applies functions correctly" $ do
      let add x y = x + y
          mul2 x = x * 2
          mulAndAdd x = s add mul2 x
      mulAndAdd 3 `shouldBe` 12