module Solver
where

type Puzzle = (Int,Int,[Element])
data Element= Entry|Exit|Empty|Wall|StartA|DestA
    deriving (Eq, Show)
type Coord = (Int,Int)
 
puzzle :: [String] -> Puzzle
puzzle ss = (length (head ss), length ss, map charToElement (concat ss))
    where
    charToElement 'E' = Entry
    charToElement 'X' = Exit
    charToElement '.' = Empty
    charToElement '#' = Wall
    charToElement 'a' = StartA
    charToElement 'A' = DestA


findElement :: Element -> Puzzle -> [Coord]
findElement x (w,h,es) = [(p `mod` w, p `div` w) | (p,e) <- zip [0..] es, e == x]

entry :: Puzzle -> [Coord]
entry = findElement Entry

exits :: Puzzle -> [Coord]
exits = findElement Exit




