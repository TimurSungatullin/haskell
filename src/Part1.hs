module Part1
  ( prob1
  , prob2
  , prob3
  , prob4
  , prob5
  ) where

------------------------------------------------------------
-- PROBLEM #1
--
-- Реализовать функцию, которая возвращает остаток от
-- деления на 65537 суммы утроенного своего аргумента
-- и числа 123
--
-- На вход функции подаются неотрицательные числа
prob1 :: Int -> Int
prob1 x = (3 * x + 123) `mod` 65537


------------------------------------------------------------
-- PROBLEM #2
--
-- Реализовать функцию, которая:
-- * нечётные числа увеличивает втрое и добавляет единицу
-- * чётные числа делит на два
prob2 :: Integer -> Integer
prob2 num
    | even num = num `div` 2
    | odd num = num * 3 + 1


------------------------------------------------------------
-- PROBLEM #3
--
-- Реализовать функцию, которая принимает функцию step,
-- положительное число n и пока текущее число не станет
-- равно единице:
-- * вызывает step с текущим числом для получения
--   следующего числа
-- * если текущее число -- единица, возвращает количество
--   выполненных шагов
--
-- Например, если в качестве step используется уменьшение
-- на единицу, а в качестве n передать 5, то должно быть
-- возвращено 4, поскольку последовательность будет такой:
--    5 -> 4 -> 3 -> 2 -> 1
--
-- Если в качестве step передать решение prob2, а n == 3,
-- то ответ 7, а последовательность такая:
--    3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- Для любой функции step и n == 1 ответом будет 0.
prob3 :: (Integer -> Integer) -> Integer -> Integer
prob3 step n = counter n 0
    where
        counter :: Integer -> Integer -> Integer
        counter 1 count = count
        counter currentNum count = counter
            (step currentNum)
            (succ count)


------------------------------------------------------------
-- PROBLEM #4
--
-- Реализовать функцию, возвращающую n-е число Фибоначчи.
-- Нулевое число равно 1, первое тоже 1. Каждое последующее
-- равно сумме двух предыдущих.
--
-- Число n может быть отрицательным, последовательность
-- продолжается естественным образом: (-1)-е число равно 0,
-- далее (-2)-е равно 1, (-3)-е равно (-1), (-4)-е равно 2
-- и т.д. -- сохраняется свойство, что последующие числа
-- равны сумме двух предыдущих.
--
-- Число n по модулю не превосходит 10^5
prob4 :: Integer -> Integer
prob4 n
    | n >= 0 = positive 1 1 n
    | otherwise = negative 1 1 n
    where
        positive first second current
            | current == 0 = first
            | otherwise = positive second (first + second) (pred current)

        negative first second current
            | current == 0 = second
            | otherwise = negative second (first - second) (succ current)


------------------------------------------------------------
-- PROBLEM #5
--
-- Написать функцию, возвращающую True, если все простые
-- делители первого аргумента n меньше второго аргумента k
--
-- Числа n и k положительны и не превосходят 10^8.
-- Число 1 не считается простым числом
getDivisorsWithCurrent :: Integer -> Integer -> [Integer]
getDivisorsWithCurrent _ 1 = []
getDivisorsWithCurrent divisor number
    | divisor * divisor > number = [number]
    | number `mod` divisor == 0 = divisor : getDivisorsWithCurrent divisor (number `div` divisor)
    | otherwise = getDivisorsWithCurrent (divisor + 1) number
prob5 :: Integer -> Integer -> Bool
prob5 n k = all (<k) $ getDivisorsWithCurrent 2 n
