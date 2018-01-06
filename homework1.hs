module Homework1 where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise =
    let
      remainder =
        mod n 10
      nextDivisor =
        div n 10
    in
      toDigits nextDivisor ++ [remainder]

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse 0 = []
toDigitsReverse n
  | n < 0 = []
  | otherwise =
    let
      remainder =
        mod n 10
      nextDivisor =
        div n 10
    in
      [remainder] ++ toDigitsReverse nextDivisor

doubleEveryOtherOne :: [Integer] -> [Integer]
doubleEveryOtherOne [] = []
doubleEveryOtherOne (x:[]) = [x]
doubleEveryOtherOne (x:y:xs) = x : y * 2 : doubleEveryOtherOne xs

--reverseList :: [a] -> [a]
--reverseList [] = []
--reverseList (x:xs) = reverseList xs : x
reverseList :: [a] -> [a]
reverseList list = go [] list
    where
        go acc [] = acc
        go acc (x:xs) = go (x:acc) xs

getValue :: Integer -> Integer
getValue num
  | num > 9 =
      let
        remainder =
          mod num 10
        nextDivisor =
          div num 10
      in
        getValue nextDivisor + remainder
  | otherwise =
      num

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =
  getValue x + sumDigits xs

validate :: Integer -> Bool
validate num =
  sumDigits (reverseList (doubleEveryOtherOne (toDigitsReverse num))) `mod` 10 == 0
