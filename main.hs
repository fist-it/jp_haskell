import GHC.Conc (pseq)
import GHC.Exts.Heap (ClosureType(BCO))
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

-- zadanie 9:
-- dla danej liczby naturalnej n podaj dla jakiej liczby naturalnej m <= n
-- zaczyna sie najdluzszy ciag Collatza
--
-- ciag collatza:
-- n%2 == 0 -> n/2
-- n%2 == 1 -> 3*n+1

-- main :: IO ()
-- main = do
--   putStrLn "Zadanie 38:"
