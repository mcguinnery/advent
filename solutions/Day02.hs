module Day02 where

import Data.Char
import Data.List

main :: IO (Int, Int)
main = do 
    input <- lines <$> readFile "..\\inputs\\day02input.txt"
    pure (sum $ map gameID $ filter gamePossible input, sum $ map power input)

gameID :: String -> Int
gameID = read . takeWhile isDigit . dropWhile (not.isDigit) 

lastNumber :: String -> Int
lastNumber = read . reverse . takeWhile isDigit . dropWhile (not.isDigit) . reverse

gamePossible :: String -> Bool
gamePossible str = 
    let (maxRed, maxGreen, maxBlue) = (12, 13, 14)
    in highest "red" str <= maxRed && highest "green" str <= maxGreen && highest "blue" str <= maxBlue

highest :: String -> String -> Int
highest color = maximum . map lastNumber . filter (isSuffixOf color) . inits 

power :: String -> Int
power str = (highest "red" str) * (highest "green" str) * (highest "blue" str)
