data Term = Var Char | Or Term Term | And Term Term | Then Term Term | 
				Eq Term Term | Neq Term Term -- | Termino Term 
				| Verdadero String | Falso String
				deriving Show
{-
(\/) :: Term -> Term -- ->  String

(/\) :: Term -> Term -- ->  String

(==>) :: Term -> Term -- -> String

(<==>) :: Term -> Term -- -> String

(!<==>) :: Term -> Term -- -> String
--t1 !<==> t2 =
-}

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
true = Verdadero "true"


false :: Term
false = Falso "false"

