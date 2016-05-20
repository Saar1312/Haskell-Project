module Func
    where

import Term
import Theorems

--Función sustitución
sust :: Term -> Sust -> Term
--Ss
sust (Var y) (Ss (t, Var v)) = if v == y then t else (Var y)
--Sd
sust (Var y) (Sd (t,Ss (t1, Var v),Var v1)) | v == y = t
                                            | v1 == y = t1
                                            | otherwise = (Var y)

--St
sust (Var y) (St (t,t1,Ss (t2, Var v),Var v1,Var v2)) | v == y = t
                                                      | v1 == y = t1
                                                      | v2 == y = t2
                                                      | otherwise = (Var y)
-- \/
sust (Or t1 t2) s = Or (sust t1 s) (sust t2 s)
-- /\
sust (And t1 t2) s = And (sust t1 s) (sust t2 s)
-- true
sust Verdadero _ = Verdadero
-- false
sust Falso _ = Falso
-- Then
sust (Then t1 t2) s = Then (sust t1 s) (sust t2 s)
-- Eq
sust (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)
-- Ne
sust (Ne t1 t2) s = Ne (sust t1 s) (sust t2 s)
-- not
sust (Not t1) s = Not $ sust t1 s
--Error
sust _ _ = error "No es posible sustituir una expresion"

--Función instanciación
instantiate :: Equation -> Sust -> Equation
instantiate (Ecu t1 t2) s = Ecu (sust t1 s) (sust t2 s)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Ecu t1 t2) e z = Ecu (sust e (t1=:z)) (sust e (t2=:z))

infer :: Float -> Sust -> Term -> Term -> Equation
infer num s z e = leibniz (instantiate (prop num) s) e z

step :: Term -> Float -> Sust -> Term -> Term -> Term
step term1 num s z e 
    | izq == term1 = der
  | der == term1 = izq
  | otherwise = error "No es posible aplicar el teorema"
  where 
    (Ecu der izq) = (infer num s z e)


--statement :: Float -> t -> s -> t -> t -> Term -> Term -> Term -> (Term -> IO Term)
--statement num _ s _ _ z e = \term1 -> step term1 num (toS s) z e

--ToSust
toS :: a -> Sust
toS (t, s, v ) = Sd (t, s, v)
toS (t, t1, s, v, v1) = St (t, t1, s, v, v1)
--Funciones Dummys

    -- Ejemplos: step ((((p\/q)<==>(p\/q))<==>(r<==>r))/\neg r) 3.2 (Sd(p\/q,r=:p,q)) s (s/\neg r)
          --step ((p <==> (p <==> q)) <==> q) 3.1 (Sd(p,q=:q,r)) s (s<==>q)