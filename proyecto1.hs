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
showTerm (Or t1 t2) = "("++(showTerm t1) ++ " \\/ " ++ (showTerm t2)++")"
showTerm (And t1 t2) = "("++(showTerm t1) ++ " /\\ " ++ (showTerm t2)++")"
showTerm (Then t1 t2) = "("++(showTerm t1) ++ " ==> " ++ (showTerm t2)++")"
showTerm (Eq t1 t2) = "("++(showTerm t1) ++ " <==> " ++ (showTerm t2)++")"
showTerm (Ne t1 t2) = "("++(showTerm t1) ++ " !<==> " ++ (showTerm t2)++")"

instance Show Term where show = showTerm 

--------------------------------------------------------------

showSust :: Sust -> String
showSust (Ss (t, v)) = "[ "++show v++":="++show t++" ]"
showSust (Sd (t1,Ss (t, v),v1)) = "[ "++show v++", "++show v1++":="++show t1++", "++show t++" ]"
showSust (St (t1,t2,Ss (t, v),v1,v2)) = "[ "++show v++", "++show v1++", "++show v2++":="++show t1++", "++show t2++", "++show t++" ]"

instance Show Sust where show = showSust

-------------------------------------------------------------

showEcu :: Equation -> String
showEcu (Ecu t1 t2) = show t1 ++ " === " ++ show t2

instance Show Equation where show = showEcu

-------------------------------------------------------------


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
--Error
sust _ _ = error "No es posible sustituir una expresion"

--Función instanciación
instantiate :: Equation -> Sust -> Equation
instantiate (Ecu t1 t2) s = Ecu (sust t1 s) (sust t2 s)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Ecu t1 t2) e z = Ecu (sust e (t1=:z)) (sust e (t2=:z))

prop :: Float -> Equation
prop num 
	| num == 3.2 = (p <==> p) <==> (q <==> q) === true
	| otherwise = error "No theoreme"

infer :: Float -> Sust -> Term -> Term -> Equation
infer num s z e = leibniz (instantiate (prop num) s) e z

step :: Term -> Float -> Sust -> Term -> Term -> Term
step term1 num s z e 
    |  izq == term1 = der
	|  der == term1 = izq
	| otherwise = error "No es posible aplicar el teorema"
	where 
		(Ecu der izq) = (infer num s z e)

-- Ejemplo: step ((((p\/q)<==>(p\/q))<==>(r<==>r))/\neg r) 3.2 (Sd(p\/q,r=:p,q)) s (s/\neg r)