-- zadanie 38:
-- Liczby B-gładkie to takie, których dzielniki pierwsze są mniejsze lub równe B. Dla danego B i n ile jest liczb
-- B-gładkich nie przekraczających n? Przykładowo dla B = 5 i n = 30 początkowe liczby 5-gładkie inaczej
-- zwane liczbami Hamming’a to: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30… więc odpowiedź
-- wynosi: 18.

-- zadanie 26:
-- Palindrom 595 możemy zapisać jako sumę kwadratów kolejnych liczb naturalnych: 62 + 72 + 82 +
-- 92 + 102 + 112 + 122. Dla danego n wydrukować wszystkie palindromy mniejsze od n, które możemy
-- zapisać jako sumę kwadratów kolejnych liczb naturalnych.

-- zadanie 9:
-- dla danej liczby naturalnej n podaj dla jakiej liczby naturalnej m <= n
-- zaczyna sie najdluzszy ciag Collatza
--
-- ciag collatza:
-- n%2 == 0 -> n/2
-- n%2 == 1 -> 3*n+1
import Control.Monad ( when, forM_ )

callNext :: Int -> Int -> Int -> IO()
callNext curr sum n
    | curr < floor(sqrt(fromIntegral n)) = do
        let newCurr = curr+1
        sumOfSquares curr sum n
        callNext newCurr 0 n       
    | otherwise = return ()

sumOfSquares :: Int -> Int -> Int -> IO ()
sumOfSquares curr sum n
    | sum < n = do
        let newCurr = curr+1
            newSum  = sum + curr*curr

        when (newSum < n) $ palindromeCheckAndPrint newSum curr sum
        sumOfSquares newCurr newSum n
    | otherwise = return ()

palindromeCheckAndPrint :: Int -> Int -> Int -> IO ()
palindromeCheckAndPrint n curr sum
    | isPalindrome n = do
        print n
    | otherwise = return ()

isPalindrome :: Int -> Bool
isPalindrome n = 
    let str = show n
    in str == reverse str

printSumsOfConsecutiveSquaresThatArePalindromes :: Int -> IO ()
printSumsOfConsecutiveSquaresThatArePalindromes n = do
    callNext 1 0 n

main = printSumsOfConsecutiveSquaresThatArePalindromes 600