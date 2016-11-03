import Test.Hspec
import Sudoku

-- runhaskell reduceTests.hs

main :: IO()
main = hspec $ do
  describe "fixed" $ do
    it "keeps every fixed entry from a list of choices" $ do
      fixed ([[]]::[[Char]]) `shouldBe` []
      fixed [['1']] `shouldBe` ['1']
      fixed [['1','2']] `shouldBe` []
      fixed [['1','2'],['3']] `shouldBe` ['3']

  describe "remove" $ do
    it "keeps the choice if there is only one possibility" $ do
      remove ([]::[Char]) ([]::[Char]) `shouldBe` [] -- impossible but we don't care
      remove ['1'] ['1'] `shouldBe` ['1'] -- impossible but we don't care
      remove ['1'] ['2'] `shouldBe` ['2']
      remove ['2'] ['1','2'] `shouldBe` ['1']
    it "remove the elements of the first list from the second" $ do
      remove ['1','2'] ['1'..'9'] `shouldBe` ['3'..'9'] -- impossible but we don't care

  describe "reduce" $ do
    it "removes fixed entry from a group (row, col, box) of choices" $ do
      reduce ([[]]::[[Char]]) `shouldBe` [[]]
      reduce [['a']] `shouldBe` [['a']]
      reduce ["12","2","3","4","5","6","7","8","9"] `shouldBe` ["1","2","3","4","5","6","7","8","9"]
      reduce ["12","2","3","4","15"] `shouldBe` ["1","2","3","4","15"]
