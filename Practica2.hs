{-
- Lógica computacional 2020-2
- Practica 2
- Alumno: Daniel Villega Aguilar
- Número de cuenta: 417047238
-Correo: daniel_villegas@cienacias.unam.mx
- Alumno:
- Número de cuenta:  
-Correo: 
-}

module Practica2 where

-- ---------------------------------------------------------------------
-- Definimos los siguientes tipos de datos:
-- Prop para representar las fórmulas proposicionales usando los
-- constructores T, F, Var, Neg, Conj, Disy, Impl y Equi para las fórmulas
-- atómicas, negaciones, conjunciones, implicaciones y equivalencias,
-- respectivamente.
-- ---------------------------------------------------------------------

data Prop = T | F | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Equi Prop Prop deriving Eq

type Estado = [String]

instance Show Prop where
         show T = "Verdadero"
         show F = "Falso"
         show (Var p) = p
         show (Neg p) = "¬" ++ show p
         show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
         show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
         show (Impl p q) = "(" ++ show p ++ " ⟶  " ++ show q ++ ")"
         show (Equi p q) = "(" ++ show p ++ " ⟷  " ++ show q ++ ")"


-- ---------------------------------------------------------------------
-- Definimos las siguientes fórmulas proposicionales
-- como variables atómicas: p, q, r, s, t, u.
-- ---------------------------------------------------------------------
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


-- ---------------------------------------------------------------------
-- Símbolos proposicionales de una fórmula --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
-- variables :: Prop -> [String]
-- tal que (variables f) es el conjunto formado por todos los
-- símbolos proposicionales que aparecen en f. Por ejemplo,
-- >variables (Impl (Conj (Var "p") (Var "q")) (Var "p"))
-- ["p","q"]
-- >variables Conj (Var "q") (Disy (Var "r") (Var "p")) 
-- ["q","r","p"]
-- ---------------------------------------------------------------------

variables :: Prop -> Estado
variables (Var x) = [x]
variables (Neg x) = variables x 
variables (Disy x y) = rmdups $ variables x ++ variables y
variables (Conj x y) = rmdups $ variables x ++ variables y
variables (Impl x y) = rmdups $ variables x ++ variables y
variables (Equi x y) = rmdups $ variables x ++ variables y

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función 
-- conjPotencia :: [a] -> [[a]]
-- tal que (conjPotencia x) es la lista de todos los subconjuntos de x.
-- Por ejmplo,
-- >conjPotencia [1,2]
-- [[]; [2]; [1]; [1; 2]]
-- >conjPotencia []
-- [[]]
-- >conjPotencia "abc"
-- ["abc","ab","ac","a","bc","b","c",""]
-- ---------------------------------------------------------------------

conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [x: ps | ps <- (conjPotencia xs)] ++ conjPotencia xs

-- ---------------------------------------------------------------------
-- Interpretaciones --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
-- interpretacion :: Prop -> Estado -> Bool
-- tal que (interpretacion f e) es la interpretación de f en e. Por ejemplo,
-- interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p"]
-- False 
-- >interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p","q"]
-- True
-- ---------------------------------------------------------------------

--función elem toma como parámatro un elemento y una list y retorna un booleano si el elemento se encuentra contenigo
--eg: elem 1 [1,2,3] -> True

interpretacion :: Prop -> Estado -> Bool
interpretacion (Var x) xs = x `elem` xs
interpretacion (Neg x) xs = not (interpretacion x xs)
interpretacion (Disy x y) xs =  interpretacion x xs || interpretacion y xs
interpretacion (Conj x y) xs =  interpretacion x xs && interpretacion y xs
interpretacion (Impl x y) xs =  not(interpretacion x xs) || interpretacion y xs
interpretacion (Equi x y) xs =  interpretacion (Impl x y) xs && interpretacion (Impl y x) xs

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir una función que dada una fórmula proposicional,
-- la función devuelve todos los estados con los que podemos evaluar
-- la fórmula. Por ejemplo,
-- >estadosPosibles Disy (Var "q") (Conj (Var "r") (Var "q"))
-- [[],["q"]; ["r"]; ["q","r"]]
-- ---------------------------------------------------------------------

estadosPosibles :: Prop -> [Estado]
estadosPosibles (Var x) = conjPotencia [x]
estadosPosibles (Neg x) = conjPotencia (variables x)
estadosPosibles (Conj x y) = conjPotencia (variables (Conj x y))
estadosPosibles (Disy x y) = conjPotencia (variables (Disy x y))
estadosPosibles (Impl x y) = conjPotencia (variables (Impl x y))
estadosPosibles (Equi x y) = conjPotencia (variables (Equi x y))



-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir una función que dada una fórmula proposicional,
-- nos diga si es una tautología. Por ejemplo,
-- >tautologia Disy (Var "p") (Neg (Var "p"))
-- True
-- >tautologia Disy (Var "q") (Var "r")
-- False
-- ---------------------------------------------------------------------

tautologia :: Prop -> Bool
tautologia x = and [interpretacion x s | s <- estadosPosibles x]


-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir una función que dada una fórmula proposicional,
-- nos diga si es una contradicción. Por ejemplo,
-- >contradiccion Disy (Var "p") (Neg (Var "p"))
-- False
-- >contradiccion Disy (Var "q") (Var "r")
-- True
-- ---------------------------------------------------------------------

contradiccion :: Prop -> Bool
contradiccion x = not (and [interpretacion x s | s <- estadosPosibles x])


-- ---------------------------------------------------------------------
-- Modelos --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir una función que dada una interpretación y una 
-- fórmula proposicional, verifique si esta interpretación es un modelo.
-- Por ejemplo,
-- >esModelo ["r"] (Conj (Disy p q) (Disy (Neg q) r))
-- False
-- >esModelo ["p","r"] (Conj (Disy p q) (Disy (Neg q) r)) 
-- True
-- ---------------------------------------------------------------------

esModelo :: Estado -> Prop -> Bool
esModelo xs x = interpretacion x xs && True

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir una función que dada una fórmula proposicional
-- devuelve la lista de todos sus modelos; tal que (modelos f) es la 
-- lista de todas las interpretaciones de f que son modelo. Por ejemplo,
-- >modelos (Conj (Disy p q) (Disy (Neg q) r))
-- [["p","q","r"],["p","r"],["p"],["q","r"]]
-- ---------------------------------------------------------------------
--[x: ps | ps <- (conjPotencia xs)] ++ conjPotencia xs
modelos :: Prop -> [Estado]
modelos (Var x) = filter (\z -> (esModelo z (Var x)) )  (estadosPosibles (Var x))
modelos (Neg x) = filter (\z -> (esModelo z (Neg x)) ) (estadosPosibles (Neg x))
modelos (Conj x y) = filter (\z -> (esModelo z (Conj x y)) ) (estadosPosibles (Disy x y))
modelos (Impl x y) = filter (\z -> (esModelo z (Impl x y)) ) (estadosPosibles (Impl x y))
modelos (Equi x y) = filter (\z -> (esModelo z (Equi x y)) ) (estadosPosibles (Equi x y))

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir una función que dada una fórmula proposicional f
-- verifica si f es válida. Por ejemplo,
-- esValida (Impl p p)
-- True
-- esValida (Impl p q) 
-- False
-- esValida (Disy (Impl p q) (Impl q p))
-- True
-- ---------------------------------------------------------------------

esValida :: Prop -> Bool
esValida x = tautologia x

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir una función que dada una fórmula proposicional f
-- verifica si f es insatisfacible. Por ejemplo,
-- esInsatisfacible (Conj p (Neg p)) 
-- True
-- esInsatisfacible (Conj (Impl p q) (Impl q r)) 
-- False
-- ---------------------------------------------------------------------

esInsatisfacible :: Prop -> Bool
esInsatisfacible x = null (modelos x)

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir una función que dada una fórmula proposicional f
-- verifica si f es satisfacible. Por ejemplo,
-- esSatisfacible (Conj p (Neg p)) 
-- False
-- esSatisfacible (Conj (Impl p q) (Impl q r)) 
-- True
-- ---------------------------------------------------------------------

esSatisfacible :: Prop -> Bool
esSatisfacible x = not (null (modelos x))

-- ---------------------------------------------------------------------
-- Funciones auxiliares
-- Función que toma una lista y remueve los elementos duplicados
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

--Función que realiza el producto cruz recursivo de las partes una prop y un conjunto
--concAdd :: a -> [a] -> [a]
--concAdd x [] = []
--concAdd x ys = map (\ y -> x : y) ys
