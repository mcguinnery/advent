module Day01 where

import Data.Char
import Data.List

firstDigit :: String -> Int
firstDigit (x:xs) 
    | isDigit x = digitToInt x
    | otherwise = firstDigit xs

firstNumber :: String -> Int
firstNumber (x:xs)
    | isDigit x = digitToInt x
    | otherwise = case find (\(a, b) -> a `isPrefixOf` (x:xs)) writtenDigits of 
        Just (a, b) -> b
        Nothing -> firstNumber xs

lastNumber :: String -> Int
lastNumber (x:xs)
    | isDigit z = digitToInt z
    | otherwise = case find (\(a, b) -> reverse a `isPrefixOf` (z:zs)) writtenDigits of 
        Just (a, b) -> b
        Nothing -> lastNumber $ reverse zs
    where (z:zs) = reverse (x:xs)

firstAndLastDigits :: String -> Int
firstAndLastDigits str = 10 * (firstDigit str) + (firstDigit $ reverse str)

firstAndLastNumbers :: String -> Int
firstAndLastNumbers str = 10 * (firstNumber str) + (lastNumber str)

main :: IO (Int, Int)
main = do 
    input <- lines <$> readFile "..\\inputs\\day01input.txt"
    pure (sum $ map firstAndLastDigits input, sum $ map firstAndLastNumbers input)

writtenDigits :: [(String, Int)]
writtenDigits = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4), 
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
    ("zero", 0)
    ]