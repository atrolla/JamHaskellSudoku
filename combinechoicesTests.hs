import Test.Hspec
import Sudoku

-- runhaskell combinechoicesTests.hs

main :: IO()
main = hspec $ do
  describe "choice" $ do
    it "replaces a filled entry with a singleton of that value" $ do
      choice '4' `shouldBe` ['4']
    it "replaces a blank entry with all possible values" $ do
      choice '.' `shouldBe` cellValues

  describe "choices" $ do
    it "transforms a grid into a matrix of possibilities" $ do
      choices ["289.536.7"] `shouldBe` [["2","8","9","123456789","5","3","6","123456789","7"]]

  describe "combine" $ do
    it "transforms a matrix of possibilities into a list of all possible matrix" $ do
      combine [["1","2","3","4","5","6","7","8","9"]] `shouldBe` [["123456789"]]
      combine [["12","2","3","4","5","6","7","8","9"]] `shouldBe` [["123456789"],["223456789"]]

  describe "solve" $ do
    it "find a valid solution from a sudoku puzzle" $ do
      solve sudokuStep1 `shouldBe` solvedSudokuStep1

sudokuStep1 :: Grid
sudokuStep1 = [
  "753186942",
  "914237865",
  "62859.731",
  "289.536.7",
  "375861294",
  "146729358",
  "891342576",
  "462975183",
  "537618429"
  ]

solvedSudokuStep1 :: Grid
solvedSudokuStep1 = [
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
