import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Sudoku

-- runhaskell pruneTests.hs
newtype SudokuGrid = SudokuGrid { toGrid :: Grid }
    deriving Show

instance Arbitrary SudokuGrid where
    arbitrary = SudokuGrid <$> genGrid

genGrid :: Gen Grid
genGrid = vectorOf 9 $ vectorOf 9 (choose ('1','9'))

main :: IO()
main = hspec $ do
  describe "rows satisfies" $ do
    prop "rows . rows = id" $ do
      \x -> (rows . rows) (toGrid x) == toGrid (x :: SudokuGrid)

  describe "cols satisfies" $ do
    prop "cols . cols = id" $ do
      \x -> (cols . cols) (toGrid x) == toGrid (x :: SudokuGrid)

  describe "boxes satisfies" $ do
    prop "boxes . boxes = id" $ do
      \x -> (boxes . boxes) (toGrid x) == toGrid (x :: SudokuGrid)

  describe "solve2" $ do
    it "find the solution of a sudoku" $ do
      solve2 sudokuStep2 `shouldBe` solvedSudokuStep2

sudokuStep2 :: Grid
sudokuStep2 = [
  "753186942",
  "914237865",
  "62859.731",
  "289.536.7",
  "375861294",
  "146729358",
  "891342576",
  "462...183",
  "5376.8429"
  ]

solvedSudokuStep2 :: Grid
solvedSudokuStep2 = [
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
