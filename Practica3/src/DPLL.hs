{-
- Lógica computacional 2020-2
- Practica 3
- Alumno: Daniel Villegas Aguilar
- Número de cuenta: 417047238
-Correo: daniel_villegas@ciencias.unam.mx
- Alumno: Diego Alfredo Villalpando Velázquez
- Número de cuenta: 313616198
-Correo: diego.a.villalpando@ciencias.unam.mx
-}

module DPLL where

import LProp
import Data.List

-- ----------------------------------------------------------------------------
--Para representar de manera más cómoda las fórmulas, cláusulas y modelos se 
--crearon los siguientes tipos de datos:
-- ----------------------------------------------------------------------------
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- | Función que dada una Solucion regresa otra Solucion siguiendo la regla de
--  la cláusula unitaria, en otro caso regresa la Solucion de entrada.
unit :: Solucion -> Solucion
unit (mod, form) = if (null unitLiteral)
    then (mod, form)
    else (unitLiteral , (filter (\n -> not(n == unitLiteral)) form))
    where
        unitLiteral = if (null (complementNotIn (clausUnit form) (clausUnit form)))
            then []
            else (head (complementNotIn (clausUnit form) (clausUnit form)))


-- | Función que dada una Solucion regresa otra Solucion siguiendo la regla de
--  la eliminación, si es posible, en otro caso regresa la Solucion de entrada.
elim :: Solucion -> Solucion
elim (mod, form) = (mod, (literalNotInForm mod form))

-- | Función que dada una Solucion regresa otra Solucion siguiendo la regla de
--  la separación, si es posible, en otro caso regresa la Solucion de entrada.
red :: Solucion -> Solucion
red (mod, form) = (mod, (literalReducedInForm (map obtenNeg mod) form))

-- | Función que dada una Solucion regresa una lista de tipo Solucion siguiendo
--  la regla de la separación, si es posible, en otro caso regresa la Solucion
--  de entrada dentro de una lista.
split :: Solucion -> [Solucion]
split (mod, form) = if (length mod == length splitMod)
                    then [(splitMod, form)]
                    else [(splitMod, form), (negSplitMod, form)]
    where
    splitMod =  literalAddSplitForm mod form
    negSplitMod = (Neg (head splitMod)) : tail splitMod

-- |Función que para una Solucion dada regresa True si la cláusula vacía se 
--  encuentra en la fórmula, regresa False en otro caso.
conflict :: Solucion -> Bool
conflict (mod, form) = [] `elem` form

-- |Función que para una Solucion dada regresa True si la fórmula no contiene
--  causulas, regresa False en otro caso.
success :: Solucion -> Bool
success (mod, form) = null form

--[V "p", V "r", V "q", V "s"] [[V "p", V "q"], [Neg (V "p"), V "r"],[V "q", Neg (V "r"), Neg (V "p"), V "s"]]
-------------------------------------------------------------------------------
--------Funciones Auxiliares---------------------------------------------------
-------------------------------------------------------------------------------

-----AUXILIARES DE UNIT----------------
-- | Función que dada una lista de clausulas unitarias retorna las cláusulas
--   que su complemento no se encuentra en la misma lista
complementNotIn xs ys = filter (\n -> not([(obtenNeg (head n))] `elem` ys)) xs

-- | Función que dada una lista de listas te retorna la lista de clausulas unitarias
clausUnit xs = filter (\n -> length n == 1) xs

------AUXILIARES DE ELIM---------------
-- | Función que dada una lista de literales y una clausula retorna True
--   si esta no contiene a la literal
literalNotInClaus xs ys = not (False `elem` map (\n -> not (n `elem` xs)) ys)

-- | Función que dada una lista de literales y una fórmula retorna las cláusulas
--   que no contienen a la literal
literalNotInForm xs ys = filter (\z -> (literalNotInClaus xs z)) ys

------AUXILIARES DE RED----------------
-- | Función que dada una lista de literales y una clausula retorna la parte de
--   la cláusula que no contiene a la literal
clausWithoutLiteral xs ys = filter (\n -> not (n `elem` xs)) ys

-- | Función que dada una lista de literales y una fórmula retorna las cláusulas
--   reducidas sin la literal
literalReducedInForm xs ys = map (\z -> (clausWithoutLiteral xs z)) ys

------AUXILIARES DE SPLIT--------------
-- | Función que dada una lista de literales y una clausula retorna la lista con 
--   la aparicion de la primera literal nueva que se encuentre en la cláusula o 
--   la misma lista si todas las literales de la cláusula ya son elem. de la lista
literalAddNewElems xs (y:ys) = if not (y `elem` xs) then (if checkForContradiction (y:xs)
                                                          then y:xs
                                                          else xs)
    
                                else (literalAddNewElems xs ys)
literalAddNewElems xs _      = xs

-- | Función que dada una lista de literales y una formula retorna la lista con 
--   las apariciones de literales nuevas que se encuentren en la fórmula o la misma
--   lista si todas las literales de la fórmula ya son elem. de la lista
literalAddSplitForm xs (y:ys) = if (xs == (literalAddNewElems xs y))
                                then literalAddSplitForm xs ys
                                else literalAddNewElems xs y
literalAddSplitForm xs _      = xs

-- | Función que dada una lista de literales revisa que no exista la negación de
--   alguna de estas literales en la lista, si existe la elimina, de otra forma 
--   retorna la lista normal.
checkForContradiction (x:xs) = not ((obtenNeg x) `elem` xs)
