import Test.Hspec
import Control.Exception (evaluate)
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Sudoku

-- runhaskell expandTests.hs

main :: IO()
main = hspec $ do
  describe "complete" $ do
    it "checks if a matrix of choices contains only singleton" $ do
      complete ([[[]]]::[[[Char]]]) `shouldBe` False
      complete [[['1']]] `shouldBe` True
      complete [[['1','2']]] `shouldBe` False
      complete [[['1'],['2']]] `shouldBe` True

  describe "safe" $ do
    it "checks if a matrix of choices contains duplicates" $ do
      safe ([[[]]]::[[[Char]]]) `shouldBe` True
      safe [[['1'],['1']]] `shouldBe` False
      safe [[['1','2']]] `shouldBe` True

  describe "count" $ do -- TODO remove for refacto
    it "count the length of the lists inside a matrix of lists" $ do
      count ([[[]]]::[[[Char]]]) `shouldBe` [0]
      count [[[1]]] `shouldBe` [1]
      count [[[1]],[[2]]] `shouldBe` [1,1]

  describe "minimalChoices" $ do
    it "find the minimal number > 1 from a matrix of choices" $ do
      minimalChoices [[['a','a']]] `shouldBe` 2
      evaluate (minimalChoices ([[[]]]::[[[Char]]])) `shouldThrow` anyException
      evaluate (minimalChoices [[[1]]]) `shouldThrow` anyException
      minimalChoices [[['a'..'z'],['1'..'9']]] `shouldBe` 9

  describe "expand" $ do
    it "replaces a matrix row containing choices with many matrix of each choices" $ do
      evaluate (expand ([[[]]]::[[[Char]]])) `shouldThrow` anyException
      expand [[['a'..'b']]] `shouldBe` [[["a"]],[["b"]]]
      expand [[['1'],['a'..'c']]] `shouldBe` [[["1","a"]],[["1","b"]],[["1","c"]]]

  describe "properties :" $ do
    prop "filter (<3) . concat = concat . map (filter (<3))" $ do
      \x -> (filter (<3) . concat) x == (concat . map (filter (<3))) (x :: [[Int]])

  describe "solve3" $ do
    it "find the solution of a sudoku" $ do
      solve3 sudokuStep3 `shouldBe` solvedSudokuStep3

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

count =  map length . concat
