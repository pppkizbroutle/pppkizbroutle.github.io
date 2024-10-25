module Ejemplo2 where

import Data.List

suma :: Num a => [a] -> a
suma [] = 0
suma xs = suma (init xs) + last xs

myInit :: [a] -> [a]
myInit [] = []
myInit [x] = []
myInit (x:xs) = x : myInit xs 

myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse a (x:xs) = x : a : myIntersperse a xs

data Arbol a = Vacio
             | Nodo (Arbol a) a (Arbol a)  deriving Show

hojas :: Arbol a -> Int
hojas Vacio = 0
hojas (Nodo Vacio _ Vacio) = 1
hojas (Nodo l a r) = hojas l + hojas r

arbol1 :: Arbol Int
arbol1 = Nodo (Nodo (Nodo Vacio 1 Vacio) 4 (Nodo Vacio 2 Vacio)) 5 (Nodo Vacio 3 Vacio)

data ArbolH a = Hoja a
              | NodoH (ArbolH a) (ArbolH a) deriving Show

arbol2 :: ArbolH Int
arbol2 = NodoH (NodoH (Hoja 1) (Hoja 2)) (NodoH (Hoja 3) (Hoja 4))



mifoldr :: (a -> b -> b) -> b -> ArbolH a -> b
mifoldr f b (Hoja a) = f a b
mifoldr f b (NodoH l r) = mifoldr f (mifoldr f b r) l

mifoldl :: (b -> a -> b) -> b -> ArbolH a -> b
mifoldl f b (Hoja a) = f b a
mifoldl f b (NodoH l r) = mifoldl f (mifoldl f b l) r
