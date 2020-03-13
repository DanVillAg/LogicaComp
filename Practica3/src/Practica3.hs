{-
- Lógica computacional 2020-2
- Practica 3
- Alumno: Daniel Villega Aguilar
- Número de cuenta: 417047238
-Correo: daniel_villegas@cienacias.unam.mx
- Alumno: Diego Alfredo Villalpando Velázquez
- Número de cuenta: 313616198
-Correo: diego.a.villalpando@ciencias.unam.mx
-}

module Practica3
(   Literal,
    Clausula,
    Formula,
    Modelo,
    Solucion,
    elimEquiv,
    elimImp,
    meteNeg,
    dist,
    cnf,
    unit,
    red,
    split,
    conflict,
    success
) where

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

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- ----------------------------------------------------------------------------
--Para representar de manera más cómoda las fórmulas, cláusulas y modelos se 
--crearon los siguientes tipos de datos:
-- ----------------------------------------------------------------------------
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- | Función que elimina equivalencias una fórmula.
elimEquiv :: Clausula -> Clausula
elimEquiv [Equi p q] = [Disy (Conj p q) (Conj ((Neg p)) (Neg q))]
elimEquiv (x:xs)
    | x == Equi p q = [elimEquiv x : elimEquiv xs]
    | x /= Equi p q = [x : elimEquiv xs]
elimEquiv [(x:xs):xss] = [elimEquiv (x:xs) : elimEquiv xss]
{-}
-- | Función que elimina implicaciones una fórmula.
elimImp :: Clausula -> Clausula
elimImp [Impl p q] = [Disy (Neg p) (Neg q)]
elimImp (x:xs)
    | x == Impl p q = [elimImp x : elimImp xs]
    | x /= Impl p q = [x : elimImp xs]
elimImp [(x:xs):xss] = [elimImp (x:xs) : elimImp xss]

-- | Función que mete negaciones en una fórmula.
meteNeg :: Formula -> Formula
meteNeg f = error ""

-- | Función que distribuye una fórmula.
dist :: Formula -> Formula
dist f = error ""

-- | Función que convierte una fórmula a forma normal conjuntiva.
cnf :: Formula -> Formula
cnf f = error ""

-- | Función que dada una Solucion regresa otra Solucion siguiendo la regla de
--  la cláusula unitaria, en otro caso regresa la Solucion de entrada.
unit :: Solucion -> Solucion
unit s = error ""

-- | Función que dada una Solucion regresa otra Solucion siguiendo la regla de
--  la eliminación, si es posible, en otro caso regresa la Solucion de entrada.
red :: Solucion -> Solucion
red s = error ""

-- | Función que dada una Solucion regresa una lista de tipo Solucion siguiendo
--  la regla de la separación, si es posible, en otro caso regresa la Solucion
--  de entrada dentro de una lista.
split :: Solucion -> [Solucion]
split s = error ""

-- |Función que para una Solucion dada regresa True si la cláusula vacía se 
--  encuentra en la fórmula, regresa False en otro caso.
conflict :: Solucion -> Bool
conflict s = error ""

-- |Función que para una Solucion dada regresa True si la fórmula no contiene
--  causulas, regresa False en otro caso.
success :: Solucion -> Bool
success s = error ""

-- ----------------------------------------------------------------------------
-- Funciones auxiliares
-------------------------------------------------------------------------------

-- |Función que dada una fórmula con una negación de proposiciones, regresa la
--  negación de cada proposición
elimNeg :: Clausula -> Clausula
elimNeg [Neg (Conj p q)] = [Disy (Neg p) (Neg q)]
elimNeg [Neg (Disy p q)] = [Conj (Neg p) (Neg q)]
elimNeg (x:xs)
    | x == Neg (Conj p q) || x == Neg (Disy p q) = [elimNeg x : elimNeg xs]
    | otherwise = [x : elimNeg xs]

-- | Función que dada una fórmula dada regresa una fórmula sin dobles 
--  negaciones.
elimDoNeg :: Clausula -> Clausula
elimDoNeg [Neg (Neg p)] = [p]
elimDoNeg (x:xs)
    | x == Neg (Neg p) = [p : elimDoNeg xs]
    | otherwise = [p : elimDoNeg]
elimDoNeg ((x:xs):xss) = [elimDoNeg (x:xs) : elimDoNeg xss]

-- | Función que convierte una fórmula a forma normal negativa.
cnn :: Formula -> Formula
cnn f = error ""

-- |Función que para una Literal dada regresa su contraria.
litContraria :: Literal -> Literal
litContraria p = error ""
-}