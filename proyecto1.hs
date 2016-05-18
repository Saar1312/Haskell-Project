data Term = Var Char
 			| Verdadero 
 			| Falso 
 			| Or Term Term 
 			| And Term Term 
 			| Then Term Term 
 			| Eq Term Term 
 			| Ne Term Term 
  			| Not Term 

data Equation = Eq1 Term Term

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
(===) t1 t2 = Eq1 t1 t2

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


i' = \x -> x
k' = \x -> \y -> x 
s' = \x -> \y -> \z -> (x z) (y z)
s'' = \x -> \y -> \z -> x (y z) 

--Función abstraer
abstraer :: Term -> Term -> (Term -> Term)
abstraer (Var x) (Var y) = if x == y then i' else k' (Var y)
-- \/
abstraer (Var x) (Or t1 t2) = s' (s' (k' Or) (abstraer (Var x) t1)) (abstraer (Var x) t2)
-- /\
abstraer (Var x) (And t1 t2) = s' (s' (k' And) (abstraer (Var x) t1)) (abstraer (Var x) t2)
-- true
abstraer _ Verdadero = k' Verdadero
-- false
abstraer _ Falso = k' Falso
-- Then
abstraer (Var x) (Then t1 t2) = s' (s' (k' Then) (abstraer (Var x) t1)) (abstraer (Var x) t2)
-- Eq
abstraer (Var x) (Eq t1 t2) = s' (s' (k' Eq) (abstraer (Var x) t1)) (abstraer (Var x) t2)
-- Ne
abstraer (Var x) (Ne t1 t2) = s' (s' (k' Ne) (abstraer (Var x) t1)) (abstraer (Var x) t2)
-- not
abstraer (Var x) (Not t1) = s'' (Not) (abstraer (Var x) t1)
-- errores
abstraer (Or t1 t2) _ = error "solo se puede abstraer una variable"
abstraer (And t1 t2) _ = error "solo se puede abstraer una variable"

--Funcion sustitución
sust :: Term -> Sust -> Term
sust t (Ss (t1, v1)) = abstraer v1 t t1
{-sust t (Sd (t1, s, v2)) = 
sust t (St (t1, t2, s, v1, v2)) = -}

--Función instanciación
instantiate :: Equation -> Sust -> Equation
instantiate (Eq1 t1 t2) s = Eq1 (sust t1 s) (sust t2 s)

--leibniz :: Equation -> Term -> Term 


			-- EXP[    var1,  var2 := exp1 , exp2   ]  
doubleSust :: Term -> Term -> Term -> Term -> Term -> Term
doubleSust (Var x) (Var y) (Var z) e1 e2 
	| x == y = e1
	| x == z = e2
doubleSust (Not e1) (Var x) (Var y) e2 e3 = Not (doubleSust e1 (Var x) (Var y) e2 e3)
doubleSust (Or e1 e2) (Var x) (Var y) e3 e4 = Or (doubleSust e1 (Var x) (Var y) e2 e3) (doubleSust e2 (Var x) (Var y) e2 e3)
doubleSust (And e1 e2) (Var x) (Var y) e3 e4 = And (doubleSust e1 (Var x) (Var y) e2 e3) (doubleSust e2 (Var x) (Var y) e2 e3)
doubleSust (Eq e1 e2) (Var x) (Var y) e3 e4 = Eq (doubleSust e1 (Var x) (Var y) e2 e3) (doubleSust e2 (Var x) (Var y) e2 e3)
doubleSust (Ne e1 e2) (Var x) (Var y) e3 e4 = Ne (doubleSust e1 (Var x) (Var y) e2 e3) (doubleSust e2 (Var x) (Var y) e2 e3)

--Obtiene una tupla de tipo Sust de un termino de la forma (x1,x2,...:=e1,e2,...)
--Acordarse de que la sustitucion es al reves: "statement 3.1 with (p <==> q,p =: p,r)"


--sust permite hacer sustituciones de expresiones usando un elemento de tipo Sust
--sust :: Term -> Sust -> Term

--instantiate permite hacer sustituciones en cosas de tipo Equation (el teorema)
--				Teorema    [x:=y]  
--instantiate :: Equation -> Sust -> Equation
--instantiate (e1 === e2) s = (sust e1 s) === (sust e2 s)

--(e1:[]) =: (e2:')':[]) = Simple (e1,e2)
--('(':e1:',':e2:[]) =: (e3:',':e4:')':[]) = Multiple (e1,e2,e3,e4)
--('(':e1:',':e2:',':e3:[]) =: (e4:',':e5:',':e6:')':[]) = Multiple (e1,e2,e3,e4,e5,e6)
