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
main = collatz