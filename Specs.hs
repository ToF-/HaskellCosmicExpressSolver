import Test.Hspec
import Solver

main = hspec $ do
  describe "a map" $ do
     let m=["###X###"
           ,"#.....#"
           ,"E.....X"
           ,"#.a.A.#"
           ,"#######"]
     describe "contains informations about a puzzle" $ do
        describe "entry" $ do
            it "gives the entry point" $ do
                entry (puzzle m)  `shouldBe` [(0,2)]
                let m=["###E###"
                      ,"#.....#"
                      ,"X.....X"
                      ,"#.a.A.#"
                      ,"#######"]
                entry (puzzle m)  `shouldBe` [(3,0)]
        describe "exits" $ do
            it "gives the exit points" $ do
                exits (puzzle m) `shouldBe` [(3,0),(6,2)]



   

        
