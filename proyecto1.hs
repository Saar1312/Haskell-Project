data Term = Var String | Or Term Term | And Term Term | Then Term Term | 
				Eq Term Term | Ne Term Term | Not Term |
				Verdadero | Falso
				--deriving Show no hace falta porque la linea instance Show ... hace lo mismo

-- Como no se puede colocar explicitamente que el primer argumento de una sustitucion
-- [a=:a/\b] tiene que ser una variable, hay que usar una funcion para no permitir cosas como
-- [a/\b=:c]
data Sust = Simple Term Term | Doble Term Term Term Term | Triple Term Term Term Term Term Term

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

---------------------- PRECENDENCIAS -----------------------
-- prefix 9 neg --If no fixity declaration is given for a particular operator, it defaults to infixl 9 
infixl 8 \/ 
infixl 8 /\ 
infixr 7 ==> 
infixl 6 <==> 
infixl 6 !<==> 

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



simpleSust :: Term -> Term -> Term -> Term
simpleSust (Var x) (Var y) e = 
	if x == y then e
	else Var x 
simpleSust (Not e1) (Var x) e = Not (simpleSust e1 (Var x) e)
simpleSust (Or e1 e2) (Var x) e = Or (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
simpleSust (And e1 e2) (Var x) e = And (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
simpleSust (Eq e1 e2) (Var x) e = Eq (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)
simpleSust (Ne e1 e2) (Var x) e = Ne (simpleSust e1 (Var x) e) (simpleSust e2 (Var x) e)


