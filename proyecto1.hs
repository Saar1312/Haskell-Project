data Term = Var Char
 			| Verdadero 
 			| Falso 
 			| Or Term Term 
 			| And Term Term 
 			| Then Term Term 
 			| Eq Term Term 
 			| Ne Term Term 
  			| Not Term 
            deriving(Eq)

data Equation = Ecu Term Term
                deriving(Eq)

data Sust = Ss (Term, Term)
			| Sd (Term, Sust, Term)
			| St (Term, Term, Sust, Term, Term) 

--Operadores	

neg :: Term -> Term
neg t = Not t
				--deriving Show no hace falta porque la linea instance Show ... hace lo mismo

-- Como no se puede colocar explicitamente que el primer argumento de una sustitucion
-- [a=:a/\b] tiene que ser una variable, hay que usar una funcion para no permitir cosas como
-- [a/\b=:c]
-- Triple sobra porque el tipo es recursivo (metiendo una tupla Simple en el Sust de Doble sale Triple)

(\/) :: Term -> Term ->  Term
(\/) t1 t2 = Or t1 t2

(/\) :: Term -> Term ->  Term
(/\) t1 t2 = And t1 t2

(==>) :: Term -> Term -> Term
(==>) t1 t2 = Then t1 t2

(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Eq t1 t2

(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = Ne t1 t2

(===) :: Term -> Term -> Equation
(===) t1 t2 = Ecu t1 t2

(=:) :: Term -> Term -> Sust
(=:) t v = Ss (t, v)

---------------------- PRECENDENCIAS -----------------------
-- prefix 9 neg --If no fixity declaration is given for a particular operator, it defaults to infixl 9 
infixl 8 \/ 
infixl 8 /\ 
infixr 7 ==> 
infixl 6 <==> 
infixl 6 !<==> 
infixl 1 =:
infixl 0 ===

-- Para debuggear
printTerm :: Term -> IO ()
printTerm (Var t) = putStrLn "Var"
printTerm (Or t1 t2) = putStrLn "Or"
printTerm (And t1 t2) = putStrLn "And"
printTerm (Eq t1 t2) = putStrLn "Equivalencia"

------------------------------------------------------------
showTerm :: Term -> String
showTerm Verdadero = "true"
showTerm Falso = "false"
showTerm (Var x) = [x]-- id en vez de show para no mostrar las comillas
showTerm (Not t) = "neg " ++ (showTerm t)
showTerm (Or t1 t2) = (showTerm t1) ++ " \\/ " ++ (showTerm t2)
showTerm (And t1 t2) = (showTerm t1) ++ " /\\ " ++ (showTerm t2)
showTerm (Then t1 t2) = (showTerm t1) ++ " ==> " ++ (showTerm t2)
showTerm (Eq t1 t2) = (showTerm t1) ++ " <==> " ++ (showTerm t2)
showTerm (Ne t1 t2) = (showTerm t1) ++ " !<==> " ++ (showTerm t2)



instance Show Term where show = showTerm -- Hace que el tipo Term pertenezca a la clase de tipos Show
										 -- y que su funcion show sea showTerm

showSust :: Sust -> String
showSust (Ss (t, v)) = "[ "++show v++":="++show t++" ]"
showSust (Sd (t1,Ss (t, v),v1)) = "[ "++show v++", "++show v1++":="++show t1++", "++show t++" ]"
showSust (St (t1,t2,Ss (t, v),v1,v2)) = "[ "++show v++", "++show v1++", "++show v2++":="++show t1++", "++show t2++", "++show t++" ]"

instance Show Sust where show = showSust

showEcu :: Equation -> String
showEcu (Ecu t1 t2) = show t1 ++ " === " ++ show t2

instance Show Equation where show = showEcu


p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

true :: Term
true = Verdadero

false :: Term
false = Falso



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
sust _ _ = error "No es posible sustituir una expresion"
-- errores
--abstraer (Or t1 t2) _ = error "solo se puede abstraer una variable"
--abstraer (And t1 t2) _ = error "solo se puede abstraer una variable"

--Función instanciación
instantiate :: Equation -> Sust -> Equation
instantiate (Ecu t1 t2) s = Ecu (sust t1 s) (sust t2 s)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Ecu t1 t2) e z = Ecu (sust e (t1=:z)) (sust e (t2=:z))


toSd :: (Term, Sust, Term) -> Sust
toSd (t1, Ss(t2), t3) = Sd (t1,Ss(t2),t3)

toSt :: (Term, Term, Sust,Term, Term) -> Sust
toSt (t1, t2, Ss(t3), t4, t5) = St (t1,t2,Ss(t3),t4,t5)