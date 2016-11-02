module Sudoku where

import Data.List(transpose)

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char
gridSize :: Num a => a
gridSize = 9
boxSize :: Num a => a
boxSize = 3
cellValues :: [Char]
cellValues = ['1'..'9']
isBlank :: Digit -> Bool
isBlank = (=='.')

solve :: Grid -> Grid
solve = head . filter correct . combine . choices

correct :: Grid -> Bool
correct grid = all noDuplicates (rows grid) &&
          all noDuplicates (cols grid) &&
          all noDuplicates (boxes grid)

noDuplicates :: Eq a => [a] -> Bool
noDuplicates = undefined -- TODO

rows :: Matrix a -> Matrix a
rows = id
cols :: Matrix a -> Matrix a
cols = transpose
boxes :: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map cols . group . map group
group :: [a] -> [[a]]
group = undefined  -- TODO
ungroup :: [[a]] -> [a]
ungroup = undefined -- TODO

combine = undefined

choices = undefined

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
