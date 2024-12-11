module Main where
import Debug.Trace (trace)
import Data.Set (Set)
import Data.List (find, tails)
import qualified Data.Set as Set
import Data.Maybe (fromJust)

-- 19

-- Returns the number of digits in a decimal number
digitsInDecimal :: Int -> Int
digitsInDecimal n = floor (logBase 10 (fromIntegral n)) + 1

-- Returns the smallest power of 10 that is greater than or equal to n
-- roundToUpperPowerOf10 :: Int -> Int
-- roundToUpperPowerOf10 n = 10 ^ (digitsInDecimal n)

roundToLowerPowerOf10 :: Int -> Int
roundToLowerPowerOf10 n = 10 ^ (digitsInDecimal n - 1)

primesUpTo :: Int -> [Int]
primesUpTo n = sieve [2..n]
    where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesUpToNDigits :: Int -> [Int]
primesUpToNDigits n = primesUpTo $ (10 ^ n) - 1


--primesFromTo :: Int -> Int -> [Int]
--primesFromTo a b = filter (a<=) $ primesUpTo b


rotations :: Int -> [Int]
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

rotationsSet :: Int -> Set Int
rotationsSet n = Set.fromList $ rotations n

rotationPrimesUpTO :: Int -> [Int]
rotationPrimesUpTO n =
    let
        primes = Set.fromList $ primesUpToNDigits $ digitsInDecimal n

        setIsPrime :: Set Int -> Bool
        setIsPrime subSet = (Set.isSubsetOf subSet primes)
    in
        filter (setIsPrime . rotationsSet) $ primesUpTo n

-- 21
biggestPalindromeFromMultiplyingTwoNDigitNumbers :: Int -> Int
biggestPalindromeFromMultiplyingTwoNDigitNumbers n =
    let
        isPalindrome n = show n == reverse (show n)

        lower = 10 ^ (n - 1)
        upper = 10 ^ n - 1
        pairs = [(x, y) | x <- [upper, upper-1..lower], y <- [x, x-1..lower]]

        findBiggestPalindrome :: [(Int, Int)] -> Int
        findBiggestPalindrome [] = 0
        findBiggestPalindrome ((x, y):xs) =
            -- trace (show x ++ " * " ++ show y ++ " = " ++ show (x * y)) $
            if isPalindrome (x * y) then
                x * y
            else
                findBiggestPalindrome xs
    in
        findBiggestPalindrome pairs

--biggestNotMultipleOfAbundantNumberUpTo :: Int -> Int
-- biggestNotMultipleOfAbundantNumberUpTo n =
--     let
--         properDivisors x = filter (\y -> x `mod` y == 0) [1..x-1]
--         isAbundant x = x < sum (properDivisors x)

--         abundantNumbers = filter isAbundant [n,n-1..12]
--         abundantNumSums = Set.fromList [x+y | (x:ys) <- tails abundantNumbers, y <- ys, x+y <= n]
--     in
--         fromJust $ find (not . (`Set.member` abundantNumSums)) [n,n-1..1]
biggestNotMultipleOfAbundantNumberUpTo n =
    let
        properDivisors x = filter (\y -> x `mod` y == 0) [1..x-1]
        isAbundant x = x < sum (properDivisors x)

        abundantNumbers = filter isAbundant [n,n-1..12]
        abundantNumSums = Set.fromList [x+y | (x:ys) <- tails abundantNumbers, y <- ys, x+y <= n]
    in
        fromJust $ find (not . (`Set.member` abundantNumSums)) [n,n-1..1]

main =
    -- 14 -- Biggest number that is not a multiple of a abundant number, that is smaller than n
    print $ biggestNotMultipleOfAbundantNumberUpTo 1000
    -- putStrLn . show $ 100

    -- 19 -- Primes that are prime in all rotation permutations up to n=1000
    -- putStrLn . show . rotationPrimesUpTO $ 1000

    -- 21 -- Biggest palindrome from multiplying two n-digit numbers
    -- putStrLn . show $ biggestPalindromeFromMultiplyingTwoNDigitNumbers 6
