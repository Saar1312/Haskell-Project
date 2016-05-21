module Term
    where

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
infixl 8 \/ 
infixl 8 /\ 
infixr 7 ==> 
infixl 6 <==> 
infixl 6 !<==> 
infixl 0 ===
------------------------------------------------------------
--ShowTerm
showTerm :: Term -> String
showTerm Verdadero = "true"
showTerm Falso = "false"
showTerm (Var x) = [x]-- id en vez de show para no mostrar las comillas
showTerm (Not t) = "("++"neg " ++ (showTerm t)++")"
showTerm (Or t1 t2) = "("++(showTerm t1) ++ " \\/ " ++ (showTerm t2)++")"
showTerm (And t1 t2) = "("++(showTerm t1) ++ " /\\ " ++ (showTerm t2)++")"
showTerm (Then t1 t2) = "("++(showTerm t1) ++ " ==> " ++ (showTerm t2)++")"
showTerm (Eq t1 t2) = "("++(showTerm t1) ++ " <==> " ++ (showTerm t2)++")"
showTerm (Ne t1 t2) = "("++(showTerm t1) ++ " !<==> " ++ (showTerm t2)++")"

instance Show Term where show = showTerm 

--------------------------------------------------------------
--ShowSust
showSust :: Sust -> String
showSust (Ss (t, v)) = show v++" := "++show t
showSust (Sd (t1,Ss (t, v),v1)) = "("++show v++", "++show v1++" := "++show t1++", "++show t++")"
showSust (St (t1,t2,Ss (t, v),v1,v2)) = "("++show v++", "++show v1++", "++show v2++" := "++show t1++", "++show t2++", "++show t++")"

instance Show Sust where show = showSust

-------------------------------------------------------------
--ShowEcu
showEcu :: Equation -> String
showEcu (Ecu t1 t2) = show t1 ++ " === " ++ show t2

instance Show Equation where show = showEcu

-------------------------------------------------------------
--Funciones dummy
lambda :: String
lambda = "lambda"

with :: String
with = "with"

using :: String
using = "using"
-------------------------------------------------------------
--Funciones a-z, true y false
a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'

true :: Term
true = Verdadero

false :: Term
false = Falso