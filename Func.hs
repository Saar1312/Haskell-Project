{-# LANGUAGE FlexibleInstances #-}
module Func
	where

import Term
import Theorems

--Clase Sustitución
-----------------------------------------------------------------------------
class Sustitucion i where
	toSust :: i -> Sust

instance Sustitucion Sust where
	toSust x = x

instance Sustitucion (Term,Sust,Term) where
	toSust (t2,s,t3) = Sd (t2,s,t3)

instance Sustitucion (Term,Term,Sust,Term,Term) where
	toSust (t2,t3,s,t4,t5) = St (t2,t3,s,t4,t5)
-----------------------------------------------------------------------------

--Función sustitución
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------

--Instanciación
instantiate :: Equation -> Sust -> Equation
instantiate (Ecu t1 t2) s = Ecu (sust t1 s) (sust t2 s)

--Leibniz
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Ecu t1 t2) e z = Ecu (sust e (t1=:z)) (sust e (t2=:z))

--Inferencia
infer :: Float -> Sust -> Term -> Term -> Equation
infer num s z e = leibniz (instantiate (prop num) s) e z

--Step
step :: Term -> Float -> Sust -> Term -> Term -> Term
step term1 num s z e 
	| izq == term1 = der
	| der == term1 = izq
	| otherwise = error "No es posible aplicar el teorema"
  where 
	(Ecu der izq) = (infer num s z e)

-- Proof
proof :: Equation -> IO Term
proof (Ecu t1 t2) = 
	do 
		putStrLn $ showTerm t1
		return t1

--Done
done :: Equation -> (Term -> IO ())
done (Ecu t1 t2) = \term -> 
	if (t2 == term) then putStrLn "Prueba exitosa"
	else putStrLn "No se ha podido demostrar el teorema"

--Statement
statement :: Sustitucion s => Float -> t -> s -> t1 -> t2 -> Term -> Term -> (Term -> IO Term)
statement num _ s _ _ z e = \term1 -> 
	do
		let teorema = step term1 num (toSust s) z e
		putStrLn $ "=== <statement "++show num ++ " with "++ showSust(toSust s)++" using lambda " ++ show z++"."++ showTerm e++">"
		putStrLn $ (showTerm teorema)
		return teorema