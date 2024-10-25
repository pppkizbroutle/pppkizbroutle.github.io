-- Comentario sdjksdhkjsahdksadkdjshka
module Funciones where

{-
jkdfjkdfjkdfjdkjfdkfj
dfdjfjfdkfjdk
-}

-- | Holas goalsdksalddjfadkhk
bool1 :: Bool
bool1 = True

and1 :: Bool -> Bool
and1 b = b && False 

and2 :: Bool -> Bool
and2 b = b && True

or1 :: Bool -> Bool
or1 b = b || True

menorque3 :: Int -> Bool
menorque3 var = var < 3

mayorque5 :: Int -> Bool
mayorque5 x = x > 5

menorigual5 :: Int -> Bool
menorigual5 var = var <= 5

mayorigual3 :: Int -> Bool
mayorigual3 x = x >= 3

g :: Int -> Int -> Int
g x y = x + y

funcionCon5Parametros :: Int -> Int -> Int -> Int -> Int -> Int
funcionCon5Parametros a b c d e = (((a + b) * d) - c) * e

func :: Int -> Int -> (Int, Int)
func x y = (x+y,x-y)

divide :: Float -> Float -> Float
divide x y = x / y

cond1 :: Int -> Int -> String
cond1 x y = if x < y then "El primero es menor al segundo" else "El primero no es menor al segundo"

cond2 :: Int -> Int -> String
cond2 x y = if x > y then "El primero es mayor al segundo" else if x == y then "Son iguales" else "El primero es menor al segundo"

cond3 :: Int -> Int -> String
cond3 x y
  | x < y = "El primero es menor al segundo"
  | x == y = "Son iguales"
  | otherwise = "El primero es mayor al segundo"

fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | otherwise = n * fact (n-1)
