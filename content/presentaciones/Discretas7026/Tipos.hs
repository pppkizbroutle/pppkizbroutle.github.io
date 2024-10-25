module Tipos where

data RGB = Rojo
         | Verde
         | Azul

color :: RGB -> String
color x = case x of
            Rojo -> "El color es rojo"
            Verde -> "El color es verde"

color1 :: RGB -> String
color1 Rojo = "El color es rojo"
color1 Azul = "El color es azul"

siguiente :: RGB -> RGB
siguiente Rojo = Verde
siguiente Verde = Azul
siguiente Azul = Rojo

siguienteColor :: RGB -> String
siguienteColor x = case siguiente x of
                     Rojo -> "El color que sigue es el rojo"
                     Verde -> "El color que sigue es el verde"
                     Azul -> "El color que sigue es el azul"

data Combinacion = Comb RGB RGB

rojoVerde :: Combinacion
rojoVerde = Comb Rojo Azul

primero :: Combinacion -> RGB
primero (Comb primerColor segundoColor) = primerColor

data NumeroComplejo = Complejo Double Double

parteReal :: NumeroComplejo -> Double
parteReal (Complejo r i) = r

data Natural = Cero
             | Suc Natural deriving Show

sumaNat :: Natural -> Natural -> Natural
sumaNat Cero m = m
sumaNat (Suc n) m = Suc (sumaNat n m)

natAInt :: Natural -> Int
natAInt Cero = 0
natAInt (Suc n) = natAInt n + 1

intANat :: Int -> Natural
intANat 0 = Cero
intANat x = Suc (intANat (x - 1))

vacia :: [Int] -> Bool
vacia [] = True
vacia _ = False

individual :: [Int] -> Bool
individual [_] = True
individual _ = False

cabeza :: [Int] -> Int
cabeza (x:xs) = x


