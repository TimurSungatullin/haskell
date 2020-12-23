module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 colorLetter = case colorLetter of
    RED   -> 'R'
    GREEN -> 'G'
    BLUE  -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
colorToInt :: ColorPart -> Int
colorToInt colorPart = case colorPart of
    Red int   -> int
    Green int -> int
    Blue int  -> int
prob7 colorP = colorToInt colorP >= 0 && colorToInt colorP <= 255

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color colorPart = case colorPart of
    Red   v -> color { red   = red color   + v }
    Green v -> color { green = green color + v }
    Blue  v -> color { blue  = blue color  + v }

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 colorPart = colorToInt colorPart

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color
    | blue color > red color && blue color > green color    = Just (Blue (blue color))
    | red color > green color && red color > blue color     = Just (Red (red color))
    | green color > red color && green color > blue color   = Just (Green (green color))
    | otherwise                                             = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = sum(findTree (Just tree))

findTree :: Maybe (Tree a) -> [a]
findTree Nothing = []
findTree (Just tree) = findTree (left tree) ++ [root tree] ++ findTree (right tree)

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
checker tree = checkR (right tree) (root tree) && checkL (left tree) (root tree)
prob12 tree = checker tree

checkR :: Ord a => Maybe (Tree a) -> a -> Bool
checkR Nothing x = True
checkR (Just tree) parent = root tree >= parent && checker tree

checkL :: Ord a => Maybe (Tree a) -> a -> Bool
checkL Nothing x = True
checkL (Just tree) parent = root tree < parent && checker tree

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 a tree = findValue a (Just tree)

findValue :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
findValue a Nothing = Nothing
findValue a (Just tree)
    | a > root tree = findValue a (right tree)
    | a < root tree = findValue a (left tree)
    | otherwise = Just tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 tree = changeToInt tree 1

getRootNum :: Maybe (Tree Int) -> Int -> Int
getRootNum Nothing x = x
getRootNum (Just tree) x = root tree + 1

changeToInt :: Tree () -> Int -> Tree Int
changeToInt tree currentNum = tree1
    where
        tree1 = case getTree (Just tree) currentNum of
            Just a -> a
            Nothing -> Tree {left = Nothing, root = currentNum, right = Nothing}

getTree :: Maybe (Tree ()) -> Int -> Maybe (Tree Int)
getTree Nothing x = Nothing
getTree (Just tree) currentNum = Just (Tree {left = leftTree, root = num, right = rightTree})
  where
    rightTree = getTree (right tree) currentNum
    rightNum = getRootNum rightTree currentNum

    leftTree = getTree (left tree) rightNum
    num = getRootNum leftTree rightNum


------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree rotateLeft (right tree)
    where
        rotateLeft rightTree = rightTree { left = Just oldRoot }
            where
               oldRoot = tree { right = left rightTree }


------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rightRotation (left tree)
    where
        rightRotation leftSubTree = leftSubTree { right = Just oldRoot }
            where
                oldRoot = tree { left = right leftSubTree }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 tree = case buildBalanced (findTree (Just tree)) of
    Just a -> a
    Nothing -> tree

buildBalanced :: [a] -> Maybe (Tree a)
buildBalanced [] = Nothing
buildBalanced treeList =
    Just (Tree
        (buildBalanced $ take half treeList)
        (treeList !! half)
        (buildBalanced $ drop (half + 1) treeList))
  where
    half = length treeList `quot` 2
