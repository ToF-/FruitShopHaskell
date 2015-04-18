import Test.Hspec
import FruitShop

main = hspec $ do
    describe "Fruit Shop process" $ do
        it "should add product prices and sum" $ do
            process ["Pommes"] `shouldBe` [100]
            process ["Pommes","Cerises"] `shouldBe` [100,175]
            process ["Pommes","Cerises","Cerises"] `shouldBe` [100,175,250]
