import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)
import Sudoku

-- uncomment for part 2

-- newtype SudokuGrid = SudokuGrid { toGrid :: Grid }
--    deriving Show

-- instance Arbitrary SudokuGrid where
--    arbitrary = SudokuGrid <$> genGrid

-- genGrid :: Gen Grid
-- genGrid = vectorOf 9 $ vectorOf 9 (choose ('1','9'))

--

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
  
--  describe "choice" $ do
--    it "replaces a filled entry with a singleton of that value" $ do
--      choice '4' `shouldBe` ['4']
--    it "replaces a blank entry with all possible values" $ do
--      choice '.' `shouldBe` cellValues

--  describe "choices" $ do
--    it "transforms a grid into a matrix of possibilities" $ do
--      choices ["289.536.7"] `shouldBe` [["2","8","9","123456789","5","3","6","123456789","7"]]

--  describe "combine" $ do
--    it "transforms a matrix of possibilities into a list of all possible matrix" $ do
--      combine [["1","2","3","4","5","6","7","8","9"]] `shouldBe` [["123456789"]]
--      combine [["12","2","3","4","5","6","7","8","9"]] `shouldBe` [["123456789"],["223456789"]]

--  describe "solve" $ do
--    it "find a valid solution from a sudoku puzzle" $ do
--      solve sudokuStep1 `shouldBe` solvedSudokuStep1





-- part 2





-- describe "fixed" $ do
--    it "keeps every fixed entry from a list of choices" $ do
--      fixed ([[]]::[[Char]]) `shouldBe` []
--      fixed [['1']] `shouldBe` ['1']
--      fixed [['1','2']] `shouldBe` []
--      fixed [['1','2'],['3']] `shouldBe` ['3']

--  describe "remove" $ do
--    it "keeps the choice if there is only one possibility" $ do
--      remove ([]::[Char]) ([]::[Char]) `shouldBe` [] -- impossible but we don't care
--      remove ['1'] ['1'] `shouldBe` ['1'] -- impossible but we don't care
--      remove ['1'] ['2'] `shouldBe` ['2']
--      remove ['2'] ['1','2'] `shouldBe` ['1']
--    it "remove the elements of the first list from the second" $ do
--      remove ['1','2'] ['1'..'9'] `shouldBe` ['3'..'9'] -- impossible but we don't care

--  describe "reduce" $ do
--    it "removes fixed entry from a group (row, col, box) of choices" $ do
--      reduce ([[]]::[[Char]]) `shouldBe` [[]]
--      reduce [['a']] `shouldBe` [['a']]
--      reduce ["12","2","3","4","5","6","7","8","9"] `shouldBe` ["1","2","3","4","5","6","7","8","9"]
--      reduce ["12","2","3","4","15"] `shouldBe` ["1","2","3","4","15"]


--  describe "rows satisfies" $ do
--    prop "rows . rows = id" $ do
--      \x -> (rows . rows) (toGrid x) == toGrid (x :: SudokuGrid)

--  describe "cols satisfies" $ do
--    prop "cols . cols = id" $ do
--      \x -> (cols . cols) (toGrid x) == toGrid (x :: SudokuGrid)

--  describe "boxes satisfies" $ do
--    prop "boxes . boxes = id" $ do
--      \x -> (boxes . boxes) (toGrid x) == toGrid (x :: SudokuGrid)

--  describe "solve2" $ do
--    it "find the solution of a sudoku" $ do
--      solve2 sudokuStep2 `shouldBe` solvedSudokuStep2





 
-- part 3






--  describe "complete" $ do
--    it "checks if a matrix of choices contains only singleton" $ do
--      complete ([[[]]]::[[[Char]]]) `shouldBe` False
--      complete [[['1']]] `shouldBe` True
--      complete [[['1','2']]] `shouldBe` False
--      complete [[['1'],['2']]] `shouldBe` True

--  describe "safe" $ do
--    it "checks if a matrix of choices contains duplicates" $ do
--      safe ([[[]]]::[[[Char]]]) `shouldBe` True
--      safe [[['1'],['1']]] `shouldBe` False
--      safe [[['1','2']]] `shouldBe` True

--  describe "count" $ do -- TODO remove for refacto
--    it "count the length of the lists inside a matrix of lists" $ do
--      count ([[[]]]::[[[Char]]]) `shouldBe` [0]
--      count [[[1]]] `shouldBe` [1]
--      count [[[1]],[[2]]] `shouldBe` [1,1]

--  describe "minimalChoices" $ do
--    it "find the minimal number > 1 from a matrix of choices" $ do
--      minimalChoices [[['a','a']]] `shouldBe` 2
--      evaluate (minimalChoices ([[[]]]::[[[Char]]])) `shouldThrow` anyException
--      evaluate (minimalChoices [[[1]]]) `shouldThrow` anyException
--      minimalChoices [[['a'..'z'],['1'..'9']]] `shouldBe` 9

--  describe "expand" $ do
--    it "replaces a matrix row containing choices with many matrix of each choices" $ do
--      evaluate (expand ([[[]]]::[[[Char]]])) `shouldThrow` anyException
--      expand [[['a'..'b']]] `shouldBe` [[["a"]],[["b"]]]
--      expand [[['1'],['a'..'c']]] `shouldBe` [[["1","a"]],[["1","b"]],[["1","c"]]]

--  describe "properties :" $ do
--    prop "filter (<3) . concat = concat . map (filter (<3))" $ do
--      \x -> (filter (<3) . concat) x == (concat . map (filter (<3))) (x :: [[Int]])

--  describe "solve3" $ do
--    it "find the solution of a sudoku" $ do
--      solve3 sudokuStep3 `shouldBe` solvedSudokuStep3

-- count = undefined

-- data for tests

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

sudokuStep3 :: Grid
sudokuStep3 = [
  "7531...42",
  "91...7865",
  "..859.731",
  "289.536.7",
  "375......",
  "1467.....",
  "8.......6",
  "462...183",
  "5376.8429"
  ]

solvedSudokuStep3 :: Grid
solvedSudokuStep3 = [
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