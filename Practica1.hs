{-
- Lógica computacional 2020-2
- Practica 1
- 24 febrero 2020

- Alumno: Daniel Villegas Aguilar
- Número de cuenta: 417047238
- Correo:

- Alumno: Diego Alfredo Villalpando Velázquez
- Número de cuenta: 313616198
- Correo: diego.a.villalpando@ciencias.unam.mx
-}


module Practica1 where

import Data.Char()

-- Tipos definidos --

--Definir el tipo Complejo
newtype Complejo = Normal (Float, Float) deriving(Show, Eq)

--Forma para definir el tipo de los números naturales
--de manera recursiva iniciando desde el cero.
data Nat = Cero | Suc Nat deriving(Show,Eq)


--Definición recursiva de listas, la lista más
--pequeña es la lista vacía representada por la palabra Nula
data Lista a = Nula | Cons a (Lista a) deriving(Show,Eq)


--Definición recursiva de árboles, el árbol más pequeño es 
--el árbol vacío.
data Arbol = Vacio | Nodo Arbol Int Arbol deriving(Show,Eq)


-- Funciones principales --

--Funcion puntoMedio que dados dos puntos en el plano encuentra el punto medio entre los dos.
--Ejemplo
--Prelude>puntoMedio (-1,2) (7,6)
--(3.0,4.0)
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x1 , y1) (x2 , y2) = (x1 + (abs (x1 - x2) / 2), y1 + (abs (y1 - y2) / 2))


--Función que dada una ecuación de segundo grado encuentra las raices de esta en una
--pareja ordenada
--Prelude>raices 5 9 3
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c = (Normal(x1, i1), Normal(x2, i2))
    where d = b * b - 4 * a * c
          e = (- b) / (2 * a)
          rootD = sqrt (abs d)
          x1 = 
            if d < 0
            then e 
            else e + (rootD / (2 * a))
          x2 = 
            if d < 0
            then e 
            else e - (rootD / (2 * a))
          i1 = 
            if d < 0
            then rootD
            else 0
          i2 = 
            if d < 0
            then -rootD   
            else 0
    

--Definir la función segmento tal que (segmento m n xs) es la lista de los
--elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
--segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
--segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
--segmento 5 3 [3,4,1,2,7,9,0] == []
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = take (n-m+1) (drop (m-1) xs) 


--Definir la función extremos tal que (extremos n xs) es la lista formada
--por los n primeros elementos de xs y los n finales elementos de xs. Por ejemplo,
--extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs


--Funcion que elimina un intervalo de una lista; dados dos números y una lista,
--elimina los elementos que se encuentren en el intervalo de esos dos numeros.
--Por ejemplo,
--dIntervalos 2 4 [1,2,3,4,5,6,7] == [1,5,6,7]
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos m n xs = take (m-1) xs ++ drop n xs


--Un número natural n se denomina abundante si es menor que la suma de sus divisores
--propios, sin el mismo. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
--Definir la función numerosAbundantes tal que (numerosAbundantes n)
--es la lista de números abundantes menores o iguales que n. Por ejemplo,
--numerosAbundantes 50 == [12,18,20,24,30,36,40,42,48]
numerosAbundantes :: Int -> [Int]
numerosAbundantes 0 = []
numerosAbundantes n
  | sum(divisores n) >= n = n : numerosAbundantes (n - 1)
  | sum(divisores n) < n = numerosAbundantes (n - 1)


--Definir la función que recibe una lista y regrese una lista tal que 
--la lista resultante contiene los mismos elementos que la original pero
--sin duplicados.
--Ejemplo:
--eliminaDuplicados [1,3,1,2,3,2,1] ; [1,3,2]
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [] = []
eliminaDuplicados (x:xs) = x : eliminaDuplicados (filter (/=x) xs)


--Se define el primitivo de un número como sigue:
--Dado un número natural n, multiplicamos todos sus dígitos, 
--repetimos este procedimiento hasta que quede un solo dígito al 
--cual llamamos primitivo de n. Por ejemplo, para 327 
--327 : 3 X 2 X 7 = 42 y 4 X 2 = 8. 
--Por lo tanto, el primitivo de 327 es 8.
--Definir la función dado un número nos regrese el primitivo. 
--Ejemplo:
--primitivo 327 ; 8
primitivo :: Integer -> Integer
primitivo n
  | n <= 0 = error "No existen los primitivos del cero o negativos"
  | n < 10 = n
  | n > 9 = auxPrimitivos (fromIntegral length (digitos n)) (fromIntegral digitos n)


--Función que dadas dos listas y un Natural j, regresa una lista tal que, se encuentran 
--concatenados el i-ésimo elemento de la primer Lista con el i-ésimo elemento de la 
--segunda Lista; a partir del elemento j de cada una de las listas.
--Ejemplo:
--sipLis (Suc (Suc Cero)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nula))))) (Cons 7 (Cons 8 (Cons 9 Nula))) ==
--(Cons 2 (Cons 8 (Cons 3 (Cons 9 Nula))))
sipLis :: Nat -> Lista a -> Lista a -> Lista a
sipLis Cero [] s = s
sipLis Cero x [] = x
sipLis Cero x s = x:s
sipLis n x s = auxSipLis (count n) x s

-- Funciones auxiliares en caso de tenerlas van aquí --

--Función que devuelve una lista de los divisores de un número
divisores :: Int -> [Int]
divisores n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

--Función que devuelve una lista de digitos a partir de un número
digitos :: Int -> [Int]
digitos 0 = []
digitos n = map (\x -> read [x] :: Int) (show n)

--Función que devuelve el producto de una lista de números llevando la cuenta de los digitos
-- del resultado recursivamente, hasta devolver el primitivo del número de cuya lista de numeros
-- iniciales lo componían 
auxPrimitivos :: Integer -> [Integer] -> Integer
auxPrimitivos 1 [n] = n
auxPrimitivos _ n = auxPrimitivos (fromIntegral length (digitos (fromIntegral product n))) (product (fromIntegral digitos n))

count :: Nat -> Int
count Cero = 0
count (x:xs) = 1 + count xs 

auxSipLis :: Int -> Lista a -> Lista a -> Lista a
auxSipLis 0 n m = n:m
auxSipLis x n m 
  | x >= 1 = drop x n : drop x m 

--Suerte y no olviden seguir los lineamientos de entrega.