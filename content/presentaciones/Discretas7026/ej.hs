module Ejemplos where

import Data.List

color' :: RGB -> String
color' x = case x of
             Rojo -> "El color es rojo"
             Verde -> "El color es verde"

color1' :: RGB -> String
color1' Rojo = "El color es rojo"
color1' Verde = "El color es verde"

segundoElemento :: [Int] -> Int
segundoElemento (x:y:ys) = y

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

multiplicacion :: [Int] -> Int
multiplicacion [] = 1
multiplicacion (x:xs) = x * multiplicacion xs

une :: [Int] -> [Int] -> [Int]
une [] ys = ys
une (x:xs) ys = x : une xs ys

enumera :: Int -> Int -> [Int]
enumera x y = if x > y then [] else x : enumera (x + 1) y

enumera2 :: Int -> Int -> [Int]
enumera2 x y = if x == y then [x] else if x > y then x : enumera2 (x - 1) y else x : enumera2 (x + 1) y

enumera2' :: Int -> Int -> [Int]
enumera2' x y
  | x == y = [x]
  | x > y = x : enumera2' (x-1) y
  | otherwise =  x : enumera2' (x+1) y

data RGB = Rojo
         | Verde
         | Azul

class MiShow a where
  mishow :: a -> String

instance MiShow RGB where
  mishow Rojo = "rojo"
  mishow Verde = "verde"
  mishow Azul = "azul"

data Nat = Cero
         | Suc Nat deriving Show

type Alumno = (String, Double)

lista :: [(String,Double)]
lista = [("Erik", 5),("Raul", 7),("Hola", 10),("String1", 1)]

quitaApariciones :: Eq a => a -> [a] -> [a]
quitaApariciones a [] = []
quitaApariciones a (x:xs) = if a == x then
                              quitaApariciones a xs
                            else
                              x : quitaApariciones a xs

quitaRepetidos :: Eq a => [a] -> [a]
quitaRepetidos [] = []
quitaRepetidos (x:xs) = x : quitaRepetidos (quitaApariciones x xs)

mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (x:xs) = f x : mapear f xs

mapear2 :: (a -> b) -> (b -> c) -> [a] -> [c]
mapear2 f g [] = []
mapear2 f g (x:xs) = g (f x) : mapear2 f g xs

potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = potencia xs ++ mapear (\ys -> x : ys) (potencia xs)

potencia1 :: [a] -> [[a]]
potencia1 [] = [[]]
potencia1 (x:xs) = potencia xs ++ [ x : l | l <- potencia xs ]

suma1 :: Num a => [a] -> [a]
suma1 xs =
  let
    f x = x + 1
  in
    mapear f xs

ecuacionesCuadraticas :: Double -> Double -> Double -> (Double,Double)
ecuacionesCuadraticas a b c =
  let
    s1 = (-b + rad) / a2
    s2 = (-b - rad) / a2
    rad = sqrt (b^2 - 4 * a * c)
    a2 = 2 * a
  in
    (s1,s2)

ecuacionesCuadraticas' :: Double -> Double -> Double -> (Double,Double)
ecuacionesCuadraticas' a b c = (s1, s2)
  where
    s1 = (-b + rad) / a2
    s2 = (-b - rad) / a2
    rad = sqrt (b^2 - 4 * a * c)
    a2 = 2 * a

dividirLista :: [a] -> ([a],[a])
dividirLista [] = ([],[])
dividirLista [x] = ([x],[])
dividirLista (x:y:xs) = (x : l1, y : l2)
  where
    (l1,l2) = dividirLista xs

(~+~) :: Int -> Int -> Int
(~+~) x y = x + y

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- | reversa1 [] "abc"
-- | reversa1 ('a' : []) "bc"
-- | reversa1 ('b' : 'a' : []) "c"
-- | reversa1 ('c' : 'b' : 'a' : []) []
-- | 'c' : 'b' : 'a' : []
-- | ['c','b','a']
-- | "cba"


longitud :: Int -> [a] -> Int
longitud acc [] = acc
longitud acc (x:xs) = longitud (acc + 1) xs

sumaElems :: Num a => [a] -> a
sumaElems [] = 0
sumaElems (x:xs) = x + sumaElems xs

reversa1 :: [a] -> [a]
reversa1 [] = []
reversa1 (x:xs) = reversa xs ++ [x]

reversa2 :: [a] -> [a] -> [a]
reversa2 acc [] = acc
reversa2 acc (x:xs) = reversa2 (x : acc) xs

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [ (xs !! i) : perm | i <- [0 .. length xs - 1]
                                      , perm <- permutaciones (aux i xs)]
  where
    aux :: Int -> [a] -> [a]
    aux i [] = []
    aux i (x:xs)
      | i == 0 = xs
      | otherwise = x : aux (i-1) xs
