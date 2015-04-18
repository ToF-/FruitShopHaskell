import Test.Hspec
import FruitShop

p = "Pommes"
c = "Cerises"
b = "Bananes"

main = hspec $ do
    describe "Fruit Shop process" $ do
        it "should add product prices and sum" $ do
            process [p] `shouldBe` [100]
            process [b] `shouldBe` [150]
            process [p,c] `shouldBe` [100,175]

        it "should allow for reductions" $ do
            process [p,c,c] `shouldBe` [100,175,220]
            process [c,c,c,c] `shouldBe` [75,120,195,240]
            process [c,p,c,b,c,c,p] `shouldBe` [75,175,220,370,445,490,590]
            process [b,b] `shouldBe` [150,150]
            process [c,p,c,b,p,b,c] `shouldBe` [75,175,220,370,470,470,545]
