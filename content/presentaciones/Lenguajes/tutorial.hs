x :: Int
x = 5

x' :: Float
x' = 10.42
  
y :: Double
y = 10.4283762683

z :: Bool
z = False

c :: Char
c = 'a'

num :: Int
num = 1

f :: Int -> Int
f x = x ^ 2

add :: Int -> Int -> Int
add x y = x + y

resultado1 :: Int
resultado1 = f 2

resultado2 :: Int
resultado2 = f 3

suma3 :: Int -> Int -> Int -> Int
suma3 x y z = x + y + z

sumaCuadrados :: Int -> Int -> Int
sumaCuadrados x y = x^2 + y^2

(<+<) :: Int -> Int -> Int
(<+<) x y = x^2 + y^2

infixl 7 <+<

data Color = Rojo | Verde | Azul
  deriving Show

favorito :: Color
favorito = Verde

data Figura = Cuadrado Double
            | Rectangulo Double Double
            | Circulo Double
            deriving Show

cuadrado :: Figura
cuadrado = Cuadrado 4.2

rectangulo :: Figura
rectangulo = Rectangulo 1.2 5.5

circulo :: Figura
circulo = Circulo 2.3

data Alumno = Alumno { nombre :: String
                     , noCuenta :: String
                     , calif :: Double
                     , asistencias :: Int}
            deriving Show

limon :: Alumno
limon = Alumno { noCuenta = "123456789"
              , calif = 4.8
              , nombre = "Erik Rangel Limón"
              , asistencias = 2 }
              
data Talvez a = Nada | Solo a
  deriving Show

talvez1 :: Talvez Color
talvez1 = Nada

talvez2 :: Talvez Color
talvez2 = Solo Azul

talvez3 :: Talvez (Talvez Int)
talvez3 = Solo (Solo 5)

data Nat = Zero
         | Suc Nat
         deriving Show

cero :: Nat
cero = Zero

dos :: Nat
dos = Suc (Suc Zero)

tres :: Nat
tres = Suc (Suc (Suc Zero))

data Lista a = Vacia -- La lista vacia es una lista de elementos de
                       -- tipo a
               | Cons a (Lista a)
               deriving Show

enteros :: [] Int
enteros = 1 : 2 : 3 : 4 : 5 : []

enteros2 :: [Double]
enteros2 = [1.24,5.2,7.5,9.3]

type Calificacion = Double

calificacion1 :: Calificacion
calificacion1 = 3.5

type Clase = [Alumno]
type Paleta = [Color]

paleta1 :: Paleta
paleta1 = [Azul, Rojo]

mimax :: Int -> Int -> Int
mimax n m = if n > m then n else m

compara :: Int -> Int -> String
compara n m
  | n < m = "El primero es menor que el segundo"
  | n == m = "Son iguales"
  | otherwise = "El primero es mayor que el segundo"

describe :: Color -> String
describe color = case color of
                   Rojo -> "El color es rojo"
                   Verde -> "El color es verde"
                   _ -> "El color es azul"

describe2 :: Color -> String
describe2 Azul = "El color es azul"
describe2 Rojo = "El color es rojo"
describe2 Verde = "El color es verde"

predecesor :: Nat -> Nat
predecesor Zero = Zero
predecesor (Suc n) = n

ceroUno :: Nat -> Nat
ceroUno Zero = Suc Zero
ceroUno n = n

alMenosUno :: [a] -> Bool
alMenosUno [] = False
alMenosUno _ = True

cabeza :: [a] -> a
cabeza (x:_) = x

cola :: [a] -> [a]
cola (_:xs) = xs

segundo :: [a] -> a
segundo [] = undefined
segundo [x] = undefined
segundo (x:y:ys) = y

sumaNats :: Nat -> Nat -> Nat
sumaNats Zero m = m
sumaNats (Suc n) m = Suc (sumaNats n m)

data Bin a = Vacio
           | Nodo a (Bin a) (Bin a)
           deriving Show

hojas :: Bin a -> Int
hojas Vacio = 0
hojas (Nodo a Vacio Vacio) = 1
hojas (Nodo a l r) = hojas l + hojas r
