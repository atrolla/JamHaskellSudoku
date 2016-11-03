import Test.Hspec
import Sudoku

-- runhaskell correctTests.hs

main :: IO()
main = hspec $ do
  describe "noDuplicates" $ do
    it "checks if there is any duplicate in a list" $ do
      noDuplicates ([]::[Char]) `shouldBe` True
--      noDuplicates "1" `shouldBe` True
--      noDuplicates ['1'..'9'] `shouldBe` True
--      noDuplicates ('6':['1'..'9']) `shouldBe` False

  describe "cols" $ do
    it "gets a Matrix by its columns" $ do
      cols ([]::[[Char]]) `shouldBe` ([]::[[Char]])
      cols ([[]]::[[Char]]) `shouldBe` ([]::[[Char]])
      cols [[1]] `shouldBe` [[1]]
      cols ["1","3"] `shouldBe` ["13"]
      cols ["12","34"] `shouldBe` ["13","24"]

--  describe "group" $ do
--    it "group a row by 3" $ do
--      group ([]::[Char]) `shouldBe` []
--      group ['1'..'3'] `shouldBe` ["123"]
--      group ['1'..'9'] `shouldBe` ["123","456","789"]

--  describe "ungroup" $ do
--    it "reassemble the lists of a list" $ do
--      ungroup ([]::[[Char]]) `shouldBe` []
--      ungroup ([[]]::[[Char]]) `shouldBe` []
--      ungroup ["123","123","123"] `shouldBe` "123123123"

--  describe "boxes" $ do
--    it "gets a Matrix grouped by its 3x3 boxes" $ do
--      boxes boxMatrix `shouldBe` boxMatrixResult

--  describe "correct" $ do
--    it "checks if a grid is valid" $ do
--      correct sudokuValid `shouldBe` True
--      correct sudokuInvalid `shouldBe` False

boxMatrix = [
  "123456789",
  "123456789",
  "123456789"
  ]

boxMatrixResult = [
  "123123123",
  "456456456",
  "789789789"
  ]

sudokuValid = [
  "753186942",
  "914237865",
  "628594731",
  "289453617",
  "375861294",
  "146729358",
  "891342576",
  "462975183",
  "537618429"
  ]

sudokuInvalid = [
  "753186942",
  "914237865",
  "628594731",
  "289153647",
  "375861294",
  "146729358",
  "891342576",
  "462975183",
  "537618429"
  ]
