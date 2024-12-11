-- zadanie 38:
-- Liczby B-gładkie to takie, których dzielniki pierwsze są mniejsze lub równe B. Dla danego B i n ile jest liczb
-- B-gładkich nie przekraczających n? Przykładowo dla B = 5 i n = 30 początkowe liczby 5-gładkie inaczej
-- zwane liczbami Hamming’a to: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30… więc odpowiedź
-- wynosi: 18.

-- rozwiazanie:
-- 
-- 1. generowanie liczb pierwszycj do B metoda sita Eratostenesa
--
-- 2. znajdowanie czynnikow pierwszych Liczby
--
-- 3. filtrowanie przez sprawdzenie czy b jest wieksza lub rowna
-- od wszystkich czynnikow pierwszych n

-- Generowanie liczb pierwszych metodą Sita Eratostenesa
primes :: Int -> [Int]
primes n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Funkcja znajdująca czynniki pierwsze liczby
primeFactors :: Int -> [Int]
primeFactors n = factors n (primes n)
  where
    factors m (p:ps)
      | p * p > m     = [m | m > 1]
      | m `mod` p == 0 = p : factors (m `div` p) (p:ps)
      | otherwise     = factors m ps
    factors _ [] = []

-- filtr przez porownanie czynnikow z B
isBSmooth :: Int -> Int -> Bool
isBSmooth b num = all (<= b) (primeFactors num)

-- filtrowanie wielu liczb
allBSmooth :: Int -> Int -> [Int]
allBSmooth b n = [x | x <- [1..n], isBSmooth b x]

-- zliczanie
countBSmooth :: Int -> Int -> Int
countBSmooth b n = length (allBSmooth b n)

-- zadanie 26:
-- Palindrom 595 możemy zapisać jako sumę kwadratów kolejnych liczb naturalnych: 62 + 72 + 82 +
-- 92 + 102 + 112 + 122. Dla danego n wydrukować wszystkie palindromy mniejsze od n, które możemy
-- zapisać jako sumę kwadratów kolejnych liczb naturalnych.

-- Baza odpowiadająca za inicjowanie sprawdzania od nowego kwadratu
callNext :: Int -> Int -> Int -> IO()
callNext curr sum n
    | curr < ceiling(sqrt(fromIntegral n)) = do
        let newCurr = curr+1
        sumOfSquares curr sum n
        callNext newCurr 0 n       
    | otherwise = return ()

-- Suma kolejnych kwadratów, rekurencyjnie wywołuje samą siebie podając aktualnie obliczoną sumę dalej
sumOfSquares :: Int -> Int -> Int -> IO ()
sumOfSquares curr sum n
    | sum < n = do
        let newCurr = curr+1
            newSum  = sum + curr*curr

        palindromeCheckAndPrint newSum n
        sumOfSquares newCurr newSum n
    | otherwise = return ()

-- Sprawdź czy jest palindromem, jeżeli tak wypisz
palindromeCheckAndPrint :: Int -> Int -> IO ()
palindromeCheckAndPrint sum n
    | isPalindrome sum && sum < n = do
        print sum
    | otherwise = return ()

-- Zamiana wartości na łańcuch znaków, porównanie z odwróconym łańcuchem
isPalindrome :: Int -> Bool
isPalindrome n = 
    let str = show n
    in str == reverse str

-- Funkcja bazowa do wyświetlenia wszystkich palindromów, odpowiada również za zczytanie 'n'
printAllPalindromes :: IO ()
printAllPalindromes = do
    putStrLn "Podaj liczbę n:"
    input <- getLine
    let n = read input :: Int
    callNext 1 0 n

-- zadanie 9:
-- dla danej liczby naturalnej n podaj dla jakiej liczby naturalnej m <= n
-- zaczyna sie najdluzszy ciag Collatza
--
-- ciag collatza:
-- n%2 == 0 -> n/2
-- n%2 == 1 -> 3*n+1

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength n
  | even n = 1 + collatzLength (n `div` 2)
  | otherwise = 1 + collatzLength (3 * n + 1)

longestCollatz :: Int -> (Int, Int)
longestCollatz n = loop (1, 1) 2
  where
    loop acc@(m1, len1) current
      | current > n = acc
      | len2 > len1 = loop (current, len2) (current + 1)
      | otherwise = loop acc (current + 1)
      where
        len2 = collatzLength current

collatz :: IO ()
collatz = do
  putStrLn "Podaj liczbę n:"
  input <- getLine
  let n = read input :: Int
  let (m, length) = longestCollatz n
  putStrLn $ "Ciąg Collatza - n = " ++ show n ++ ": Liczba " ++ show m ++ " ma najdłuższy ciąg Collatza o długości " ++ show length ++ "."

main :: IO ()
main = do
    putStrLn "Zadanie 9"
    collatz
    putStrLn "Zadanie 26"
    printAllPalindromes
    putStrLn "Zadanie 38"
