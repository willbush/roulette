import Test.Hspec

main :: IO ()
main = hspec $
  describe "nothing" $
    it "does nothing" $
      True `shouldBe` True
