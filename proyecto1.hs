data Term = Var String   | Or Term Term | And Term Term | Then Term Term | 
			Eq Term Term | Ne Term Term | Not Term |
			Verdadero    | Falso
				--deriving Show no hace falta porque la linea instance Show ... hace lo mismo

-- Como no se puede colocar explicitamente que el primer argumento de una sustitucion
-- [a=:a/\b] tiene que ser una variable, hay que usar una funcion para no permitir cosas como
-- [a/\b=:c]
-- Triple sobra porque el tipo es recursivo (metiendo una tupla Simple en el Sust de Doble sale Triple)

data Sust = Simple Term Term |
			Doble (Term,Sust,Term) |
			Triple (Term,Term,Sust,Term,Term)
{-
data Sust = Simple | 
			Tripe Term Sust Term
-}
data Equation = Ecuacion Term Term

(\/) :: Term -> Term ->  Term
t1 \/ t2 = Or t1 t2

(/\) :: Term -> Term ->  Term
t1 /\ t2 = And t1 t2

(==>) :: Term -> Term -> Term
t1 ==> t2 = Then t1 t2

(<==>) :: Term -> Term -> Term
t1 <==> t2 = Eq t1 t2

(!<==>) :: Term -> Term -> Term
t1 !<==> t2 = Ne t1 t2

neg :: Term -> Term
neg t = Not t

-- Hace falta un showEquation?
(===) :: Term -> Term -> Equation
t1 === t2 = Ecuacion t1 t2
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
--showTerm (Verdadero true) = 
showTerm Verdadero = "true"
showTerm Falso = "false"
showTerm (Var x) = id x -- id en vez de show para no mostrar las comillas
showTerm (Not t) = "neg " ++ (showTerm t)
showTerm (Or t1 t2) = (showTerm t1) ++ " \\/ " ++ (showTerm t2)
showTerm (And t1 t2) = (showTerm t1) ++ " /\\ " ++ (showTerm t2)
showTerm (Then t1 t2) = (showTerm t1) ++ " ==> " ++ (showTerm t2)
showTerm (Eq t1 t2) = (showTerm t1) ++ " <==> " ++ (showTerm t2)
showTerm (Ne t1 t2) = (showTerm t1) ++ " !<==> " ++ (showTerm t2)



instance Show Term where show = showTerm -- Hace que el tipo Term pertenezca a la clase de tipos Show
										 -- y que su funcion show sea showTerm
showSust :: Sust -> String
showSust (Simple x y) = showTerm(x)++","++showTerm(y)
showSust (Doble (x,y,z)) = "("++showTerm(x)++","++showSust(y)++","++showTerm(z)++")"
showSust (Triple (x,y,z,v,w)) = "("++showTerm(x)++","++showTerm(y)++","++showSust(z)++","++showTerm(v)++","++showTerm(w)++")"

instance Show Sust where show = showSust

-- Ver si esto se puede hacer usando una funcion
a :: Term
a = Var "a"

b :: Term
b = Var "b"

c :: Term
c = Var "c"

d :: Term
d = Var "d"

e :: Term
e = Var "e"

f :: Term
f = Var "f"

g :: Term
g = Var "g"

h :: Term
h = Var "h"

i :: Term
i = Var "i"

j :: Term
j = Var "j"

k :: Term
k = Var "k"

l :: Term
l = Var "l"

m :: Term
m = Var "m"

n :: Term
n = Var "n"

o :: Term
o = Var "o"

p :: Term
p = Var "p"

q :: Term
q = Var "q"

r :: Term
r = Var "r"

s :: Term
s = Var "s"

t :: Term
t = Var "t"

u :: Term
u = Var "u"

v :: Term
v = Var "v"

w :: Term
w = Var "w"

x :: Term
x = Var "x"

y :: Term
y = Var "y"

z :: Term
z = Var "z"

true :: Term
true = Verdadero

false :: Term
false = Falso

--Obtiene una tupla de tipo Sust de un termino de la forma (x1,x2,...:=e1,e2,...)
--Acordarse de que la sustitucion es al reves: "statement 3.1 with (p <==> q,p =: p,r)"

(=:) :: Term -> Term -> Sust
t =: e = Simple t e


--sust permite hacer sustituciones de expresiones usando un elemento de tipo Sust
sust :: Term -> Sust -> Term
sust Verdadero s = Verdadero
sust Falso s = Falso
sust (Var x) (Simple (Var y) t) = 
	if x == y then t
	else Var x
sust (Var x) (Doble ((Var y),Simple (Var z) t1,t2))
	| x == y = t1
	| x == z = t2
	| otherwise = Var x
sust (Var x) (Triple ((Var y),(Var z),Simple (Var w) t1,t2,t3))
	| x == y = t1
	| x == z = t2
	| x == w = t3
	| otherwise = Var x
sust (Not t) (Doble s) = Not (sust t (Doble s))
sust (Or t1 t2) s = Or (sust t1 s) (sust t2 s)
sust (And t1 t2) s = And (sust t1 s) (sust t2 s)
sust (Eq t1 t2) s = Eq (sust t1 s) (sust t2 s)
sust (Ne t1 t2) s = Ne (sust t1 s) (sust t2 s)
sust (Then t1 t2) s = Then (sust t1 s) (sust t2 s)
--sust e x t = 


--instantiate permite hacer sustituciones en cosas de tipo Equation (el teorema)
--				Teorema    [x:=y]  
--instantiate :: Equation -> Sust -> Equation
--instantiate (e1 === e2) s = (sust e1 s) === (sust e2 s)












{-
-- Cambiar luego por el tipo de datos que propone flaviani con tuplas

simpleSust :: Term -> Term -> Term -> Term
simpleSust (Var x) (Var y) e = 
	if x == y then e
	else Var x 
simpleSust (Not e1) (Var x) e = Not (simpleSust e1 (Var x) e)
simpleSust (Or e1 e2) (Var x) e = Or (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
simpleSust (And e1 e2) (Var x) e = And (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
simpleSust (Eq e1 e2) (Var x) e = Eq (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
simpleSust (Ne e1 e2) (Var x) e = Ne (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
-- En este caso hay que imprimir un error, no?
simpleSust e1 e2 e3 = Var "Error" -- Esto es una trampa balurda. Usar Maybe o imprimir mensaje de error con monad

{-
simpleSust e (Or e1 e2) e = Nothing
simpleSust e (Not e1) e = Nothing
simpleSust e (And e1 e2) e = Nothing
simpleSust e (Eq e1 e2) e = Nothing
simpleSust e (Ne e1 e2) e = Nothing
-}
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
-}