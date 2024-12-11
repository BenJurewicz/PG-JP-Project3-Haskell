module Main where
import Debug.Trace (trace)
import Data.Set (Set)
import Data.List ( tails)
import qualified Data.Set as Set
import Data.Array

-- 19
-- Returns the number of digits in a decimal number
digitsInDecimal :: Integer -> Integer
digitsInDecimal n = floor (logBase 10 (fromIntegral n)) + 1

roundToLowerPowerOf10 :: Integer -> Integer
roundToLowerPowerOf10 n = 10 ^ (digitsInDecimal n - 1)

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesUpToNDigits :: Integer -> [Integer]
primesUpToNDigits n = primesUpTo $ (10 ^ n) - 1


rotations :: Integer -> [Integer]
rotations n =
    let
        base = 10
        -- Calculates where to place the rotated digit
        -- Has to be calculated once per number to handle 0's correctly
        shift = roundToLowerPowerOf10 n

        -- Rotates a digit once to the right
        rotate n = r * shift + q
            where (q, r) = n `divMod` base

        -- Generates rotations until the number repeats
        rotations' n = n : takeWhile (/= n) (tail $ iterate rotate n)
    in
        rotations' n

rotationsSet :: Integer -> Set Integer
rotationsSet n = Set.fromList $ rotations n

rotationPrimesUpTo :: Integer -> [Integer]
rotationPrimesUpTo n =
    let
        primes = Set.fromList $ primesUpToNDigits $ digitsInDecimal n

        setIsPrime :: Set Integer -> Bool
        setIsPrime subSet = (Set.isSubsetOf subSet primes)
    in
        filter (setIsPrime . rotationsSet) $ primesUpTo n

-- 21
biggestPalindromeFromMultiplyingTwoNDigitNumbers :: Integer -> Integer
biggestPalindromeFromMultiplyingTwoNDigitNumbers n =
    let
        isPalindrome n = show n == reverse (show n)

        lower = 10 ^ (n - 1)
        upper = 10 ^ n - 1
        pairs = [(x, y) | x <- [upper, upper-1..lower], y <- [x, x-1..lower]]

        findBiggestPalindrome :: [(Integer, Integer)] -> Integer
        findBiggestPalindrome [] = 0
        findBiggestPalindrome ((x, y):xs) =
            -- trace (show x ++ " * " ++ show y ++ " = " ++ show (x * y)) $
            if isPalindrome (x * y) then
                x * y
            else
                findBiggestPalindrome xs
    in
        findBiggestPalindrome pairs
        
-- 14
biggestNotMultipleOfAbundantNumberUpTo :: Integer -> Integer
biggestNotMultipleOfAbundantNumberUpTo n =
    let 
        sumOfProperDivisors :: Integer -> Array Integer Integer
        sumOfProperDivisors n = accumArray (+) 0 (1, n) [(j, i) | i <- [1..n `div` 2], j <- [2*i, 3*i..n]]

        divisorSums = sumOfProperDivisors n
        abundantNums = filter (\x -> divisorSums ! x > x) [1..n]

        sums = Set.fromList [x + y | (x:ys) <- tails abundantNums, y <- ys, x + y <= n]
    in 
        head [x | x <- [n, n-1..1], not (x `Set.member` sums)]

main :: IO ()
main = do
    -- 14 -- Biggest number that is not a multiple of a abundant number, that is smaller than n
    putStr "Zadanie 14: "
    print $ biggestNotMultipleOfAbundantNumberUpTo 30000

    -- 19 -- Primes that are prime in all rotation permutations up to n
    putStr "\nZadanie 19: "
    print $ rotationPrimesUpTo 5000

    -- 21 -- Biggest palindrome from multiplying two n-digit numbers
    putStr "\nZadanie 21: "
    print $ biggestPalindromeFromMultiplyingTwoNDigitNumbers 6
