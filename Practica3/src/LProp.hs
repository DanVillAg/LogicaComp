{-
- Logica Conmputacional 2020-2 
- Practica 3, LP en Haskell y FNC

- Creador: Pedro Juan Salvador Sánchez Pérez

- Alumno: Daniel Villegas Aguilar
- Número de cuenta: 417047238
-Correo: daniel_villegas@ciencias.unam.mx

- Alumno: Diego Alfredo Villalpando Velázquez
- Número de cuenta: 313616198
-Correo: diego.a.villalpando@ciencias.unam.mx
-}


module LProp where

-- | VarP. Tipo que representa el conjunto de variables proposicionales.
type VarP = String

-- | Prop. Tipo que representa el conjunto de fórmulas de la lógica
-- proposicional.
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop 


-- / se intancia la clase Show para poder especificar como queremos que se impriman la logica proposicional 
-- / TTrue en terminal se vera como "T", Neg (V "p") se vera como ~(p)
instance Show Prop where
    show TTrue = "T"
    show FFalse = "F"
    show (V x) = show x
    show (Neg p) = "~("++ show p ++")"
    show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
    show (Disy p q) = "(" ++ show p ++ " v " ++ show q ++")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++")"
    show (Equiv p q) = "(" ++  show p ++ " <-> " ++ show q ++")"

-- / se insatancia la clase Eq para poder comparar formulas proposicionales
-- TTrue == False = False
instance Eq Prop where
    (==) TTrue TTrue = True
    (==) FFalse FFalse = True
    (==) (V x) (V y) = x == y
    (==) (Neg p) (Neg q) = (==) p q
    (==) (Conj p q) (Conj r s) = (==) p r && (==) q s
    (==) (Disy p q) (Disy r s) = (==) p r && (==) q s
    (==) (Imp p q) (Imp r s) = (==) p r && (==) q s
    (==) (Equiv p q) (Equiv r s) = ((==) p r && (==) q s) || ((==) p s && (==) q r)
    (==) p q = False

    
-- / se instancia la clase Ord para poder dar orden a las formulas proposicionales 
-- ~p > p = False
instance Ord Prop where
    (<) p q = peso p < peso q
    (>) p q = peso p > peso q 
    (<=) p q = peso p <= peso q 
    (>=) p q = peso p >= peso q 
    min p q = if peso p <= peso q then p else q
    max p q = if peso p >= peso q then p else q

-- | peso. Función que dada una fórmula devuelve el número de sus conectivos.
--
-- --> peso (Conj (V 1) (Disy (V 2) (FFalse))) = 2
-- --> peso (Conj (V 1) (Disy (V 2) (Neg (V 3)))) = 3
peso :: Prop -> Int
peso (Neg p) = 1 + peso p
peso (Conj p q) = 1 + peso p + peso q
peso (Disy p q) = 1 + peso p + peso q
peso (Imp p q) = 1 + peso p + peso q
peso (Equiv p q) = 1 + peso p + peso q
peso _ = 0 

-- --> elimEquiv (Equiv (V p) (V q)) = (Conj (Imp (V p) (V q)) (Imp (V q) (V p)))
elimEquiv :: Prop -> Prop --OK
elimEquiv TTrue = TTrue
elimEquiv FFalse = FFalse
elimEquiv (V p) = V p
elimEquiv (Neg (V p)) = Neg (V p)
elimEquiv (Conj p q) = Conj p q
elimEquiv (Disy p q) = Disy p q
elimEquiv (Imp p q) = Imp p q
elimEquiv (Equiv p q) = Disy (Conj (elimEquiv p) (elimEquiv q)) (Conj (Neg (elimEquiv p)) (Neg (elimEquiv q)))

-- | elimImp. Funció que dada una fórmula devuelve su equivalente que no contiene implicaciones.
--
-- --> elimImp (Imp (V p) (Disy (V q) (FFalse))) = Disy (Neg (V p)) (Disy (V q) FFalse)
elimImp :: Prop -> Prop --OK
elimImp TTrue = TTrue
elimImp FFalse = FFalse
elimImp (V p) = V p
elimImp (Neg (V p)) = Neg (V p)
elimImp (Conj p q) = Conj p q
elimImp (Disy p q) = Disy p q
elimImp (Imp p q) = Disy (Neg (elimImp p)) (elimImp q)
elimImp (Equiv p q) = Equiv p q

-- | elimIE. Función que dada una fórmula devuelve su equivalente que no contiene implicaciones,
-- ni equivalencias.
--
elimIE :: Prop -> Prop
elimIE p = elimImp $ elimEquiv p

-- | funcion que recibe una formula de la logica proposicional y que devuelve otra formula de la logica proposicional que es logicamente
-- equivalente pero las negaciones que existen solo aplican a formulas atomicas. tambien elimina la doble negacion. la funcion supone
-- que la fomula ya no tiene implicaciones ni equivalencias.
meteNeg :: Prop -> Prop
meteNeg TTrue = FFalse
meteNeg FFalse = TTrue
meteNeg (V p) = Neg (V p)
meteNeg (Neg p) = auxMeteNeg p
meteNeg (Neg (Neg p)) = p
meteNeg (Conj p q) = Conj (meteNeg p) (meteNeg q)
meteNeg (Disy p q) = Disy (meteNeg p) (meteNeg q)
meteNeg (Imp p q) = error "meteNeg(Imp _ _)"
meteNeg (Equiv p q) = error "meteNeg(Equiv _ _)"


-- | funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal negativa,
fnn :: Prop -> Prop 
fnn p = meteNeg (elimIE p)

-- | funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente pero que distribuye la disyuncion sobre la conjuncion (Ej. p v (q ^ r) = (p v q) ^ (p v r)). la funcion supone que la formula 
-- esta en forma normal negativa
dist :: Prop -> Prop
dist TTrue = TTrue
dist FFalse = FFalse
dist (V p) = V p
dist (Neg p) = Neg (dist p)
dist (Conj p q) = Conj (dist p) (dist q)
dist (Disy p (Conj q r)) = Conj (dist (Disy p q)) (dist (Disy p r))
dist (Disy (Conj q r) p) = Conj (dist (Disy p q)) (dist (Disy p r))
dist (Imp _ _) = error "dist(Imp _ _): Fórmula no está en FNN"
dist (Equiv _ _) = error "dist(Equiv _ _): Fórmula no está en FNN"

-- | funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal conjuntiva,
cnf :: Prop -> Prop
cnf p = dist $ fnn p 

-- ----------------------------------------------------------------------------
-- Funciones auxiliares
-- ----------------------------------------------------------------------------

-- | Función auxiliar que dada una proposición con operador binario negado, 
-- regresa la solución correspondiente
auxMeteNeg :: Prop -> Prop
auxMeteNeg TTrue = FFalse
auxMeteNeg FFalse = TTrue
auxMeteNeg (V p) = Neg (V p)
auxMeteNeg (Neg p) = auxMeteNeg p
auxMeteNeg (Conj p q) = Disy (meteNeg (Neg p)) (meteNeg (Neg q))
auxMeteNeg (Disy p q) = Conj (meteNeg (Neg p)) (meteNeg (Neg q))
auxMeteNeg (Imp p q) = error "auxMeteNeg(Imp _ _)"
auxMeteNeg (Equiv p q) = error "auxMeteNeg(Equiv _ _)"