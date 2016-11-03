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

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
choice :: Digit -> [ Digit ]
choice = undefined -- TODO

combine :: Matrix [a] -> [Matrix a]
combine = undefined -- TODO

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

-- Part 2

reduce :: [[Digit]] -> [[Digit]]
reduce css = map (remove (fixed css)) css

fixed :: [[Digit]] -> [Digit]
fixed = undefined -- TODO

remove :: [Digit] -> [Digit] -> [Digit]
remove fs css = undefined -- TODO

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxes . pruneBy cols . pruneBy rows
pruneBy :: (Matrix [Digit] -> Matrix [Digit])->(Matrix [Digit] -> Matrix [Digit])
pruneBy f = f . map reduce . f -- if f . f = id

-- part 3

complete :: Matrix [Digit] -> Bool
complete = all (all single)
  where
    single :: [a] -> Bool
    single = undefined -- TODO

safe :: Matrix [Digit] -> Bool
safe = undefined -- TODO (almost like correct)

minimalChoices :: [[[a]]] -> Int
minimalChoices = undefined  -- TODO
-- minimalChoices = minimum . undefined

expand :: Matrix [Digit] -> [ Matrix [Digit] ]
expand matrix = [rowsBefore ++ [rowBeforeChoice ++ [c] : rowAfterChoice] ++ rowsAfter | c <- cellchoices]
  where
  (rowsBefore, rowOfMinimal : rowsAfter)            = undefined -- TODO
  -- identify a row containing a smallest minimal choices cell
  (rowBeforeChoice,  cellchoices  : rowAfterChoice) = undefined -- TODO
  -- identify the cell with the smallest minimal choices in the rowOfMinimal
  smallest cellValues                               = undefined -- TODO
  -- predicate to know if a cell has the smallest minimal choices

search :: Matrix [Digit] -> [Grid]
search cm = undefined
  -- | TODO
  -- | TODO
  -- | otherwise = concat (map search (expand pm))
  -- where pm = prune cm

solve3 :: Grid -> Grid
solve3 = head . search . choices

hardSudoku :: Grid
hardSudoku = [
  "..73....6",
  ".2..5..7.",
  "4....79..",
  "5....42..",
  ".1..6..8.",
  "..28....5",
  "..46....8",
  ".9..3..1.",
  "2....87.."
  ]
