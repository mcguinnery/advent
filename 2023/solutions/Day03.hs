module Day03 where

import Data.Char
import Data.List
import Data.Maybe(isJust)

type SymbolCoordinate = (Char, Int, Int) -- (symbol, line, column)
type NumberCoordinate = (Int, Int, Int, Int) -- (number, line, colStart, length)

main :: IO (Int, Int)
main = do 
    input <- lines <$> readFile "..\\inputs\\day03input.txt"
    pure (sum $ partNumbers input, sum . map (gearRatio input) $ getStars input)

allSymbolCoordinates :: [String] -> [SymbolCoordinate]
allSymbolCoordinates inp = symbolCoordinates inp 0

symbolCoordinates :: [String] -> Int -> [SymbolCoordinate]
symbolCoordinates [] _ = []
symbolCoordinates (current:remaining) line = symbolCoordinatesFromLine current line 0 ++ (symbolCoordinates remaining $ line + 1)

symbolCoordinatesFromLine :: String -> Int -> Int -> [SymbolCoordinate]
symbolCoordinatesFromLine [] _ _ = []
symbolCoordinatesFromLine (x:xs) line column
    | (isDigit x || x == '.') = symbolCoordinatesFromLine xs line $ column + 1
    | otherwise = (x, line, column) : (symbolCoordinatesFromLine xs line $ column + 1)

allNumberCoordinates :: [String] -> [NumberCoordinate]
allNumberCoordinates inp = numberCoordinates inp 0

numberCoordinates :: [String] -> Int -> [NumberCoordinate]
numberCoordinates [] _ = []
numberCoordinates (current:remaining) line = numberCoordinatesFromLine current line 0 ++ numberCoordinates remaining (line + 1)

numberCoordinatesFromLine :: String -> Int -> Int -> [NumberCoordinate]
numberCoordinatesFromLine [] _ _ = []
numberCoordinatesFromLine (x:xs) line column
    | isDigit x = (read $ takeWhile isDigit (x:xs), line, column, len) : (numberCoordinatesFromLine (drop len (x:xs)) line $ column + len)
    | otherwise = numberCoordinatesFromLine xs line $ column + 1
    where len = length $ takeWhile isDigit (x:xs)

isAdjacentToSymbol :: [SymbolCoordinate] -> NumberCoordinate -> Bool
isAdjacentToSymbol symbols number = 
    let (_, x_tgt, y_tgt, len) = number in 
    isJust $ find (\(_, x, y) -> abs (x - x_tgt) <= 1 && (y - y_tgt) >= (-1) && (y - y_tgt) <= len) symbols

partNumbers :: [String] -> [Int]
partNumbers inp = map getNumber parts
    where parts = filter (isAdjacentToSymbol $ allSymbolCoordinates inp) $ allNumberCoordinates inp

getStars :: [String] -> [SymbolCoordinate]
getStars inp = filter (\(first, _, _) -> first == '*') $ allSymbolCoordinates inp 

getSymbolName :: SymbolCoordinate -> Char
getSymbolName = \(first, _, _) -> first

getNumber :: NumberCoordinate -> Int 
getNumber = \(first, _, _, _) -> first

getAdjacentNumbers :: SymbolCoordinate -> [NumberCoordinate] -> [NumberCoordinate]
getAdjacentNumbers symbol = filter (isAdjacentToSymbol [symbol])

gearRatio :: [String] -> SymbolCoordinate -> Int
gearRatio inp star
    | length adjacentNumbers == 2 = product $ map getNumber adjacentNumbers
    | otherwise = 0
    where adjacentNumbers = getAdjacentNumbers star $ allNumberCoordinates inp


{-
adjacentNumbers :: [NumberCoordinate] -> SymbolCoordinate -> [NumberCoordinate]
adjacentNumbers numbers symbol = filter (isAdjacentToSymbol [symbol]) numbers 

gearRatio :: (SymbolCoordinate, [Int]) -> Int
gearRatio = product . (\(_, second) -> second)
          
gears :: [String] -> [(SymbolCoordinate, [Int])]
gears inp = map (\gear -> (gear, map (\(first, _, _, _) -> first) $ adjacentNumbers (numberCoordinates inp 0) gear)) $ filter isGear gearPossibles
    where gearPossibles = filter (\(first, _, _) -> first=='*') $ symbolCoordinates inp 0
          isGear = (\a -> length (adjacentNumbers (numberCoordinates inp 0) a) == 2) -}