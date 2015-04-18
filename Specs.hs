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
            process [p,c,c] `shouldBe` [100,175,230]
            process [c,c,c,c] `shouldBe` [75,130,205,260]
            process [c,p,c,b,c,c,p] `shouldBe` [75,175,230,380,455,510,610]
