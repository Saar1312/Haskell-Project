module Func
	where

import Term
import Theorems

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-
Falta:
* Revisar como hacer para correrlo sin usar el flag -XFlexibleInstances usando la linea
{.-# LANGUAGE FlexibleInstances #-.} (que no esta funcionando)
* Funciones proof y done
* Hacer que step retorne IO()
* Comentar bien
* Probar
* Hacer que showTerm no parentice las expresiones mas externas: ejemplo ((p<==>q)\/s)
-}


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

proof :: Equation -> IO Term
proof (Ecu t1 t2) = 
	do 
		putStrLn $ showTerm t1
		return t1

done :: Equation -> (Term -> IO ())
done (Ecu t1 t2) = \term -> 
	if (t2 == term) then putStrLn "Prueba exitosa"
	else putStrLn "No se ha podido demostrar el teorema"

class Inferencia i where
	toSust :: i -> Sust

instance Inferencia Sust where
	toSust x = x

instance Inferencia (Term,Sust,Term) where
	toSust (t2,s,t3) = Sd (t2,s,t3)

instance Inferencia (Term,Term,Sust,Term,Term) where
	toSust (t2,t3,s,t4,t5) = St (t2,t3,s,t4,t5)

statement :: Inferencia s => Float -> t -> s -> t1 -> t2 -> Term -> Term -> (Term -> IO Term)
statement num _ s _ _ z e = \term1 -> 
	do
		let teorema = step term1 num (toSust s) z e
		putStrLn $ "=== <statement "++show num ++ " with "++ showSust(toSust s)++" using lambda " ++ show z++"."++ showTerm e++">"
		putStrLn $ (showTerm teorema)
		return teorema