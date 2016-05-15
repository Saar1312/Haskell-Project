import Control.Monad.Identity

data Term = Var Int | Or Term Term

x1 :: Term
x1 = Var 1

x2 :: Term
x2 = Var 2

x3 :: Term
x3 = Var 3

(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

showTerm :: Term -> String
showTerm (Var i) = "x" ++ (show i)
showTerm (Or (Var i) (Var j)) = showTerm(Var i) ++ "\\/" ++ showTerm(Var j)
showTerm (Or (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

instance Show Term where show = showTerm

i = \x -> x

k = \x -> \y -> x

s = \x -> \y -> \z -> (x z) (y z)

abstraer :: Term -> Term -> (Term -> Term)
abstraer (Var x) (Var y) = if x == y then i else k (Var x)
abstraer (Var x) (Or t1 t2) = s (s (k Or) (abstraer (Var x) t1)) (abstraer (Var x) t2) 


{- ESTA PARTE NO IMPORTA TANTO AHORITA, PERO SIRBE PARA APRENDER A USAR EL >>=
data Tree t = Nil | Node t (Tree t) (Tree t)

-- Arbol de ejemplo
tr = Node 3 ((Node 3) Nil (Node 4 Nil Nil)) ((Node 3) (Node 1 Nil Nil) (Node 4 Nil Nil))

-- Sin Monad
sTree :: (Num a) => Tree a -> a
sTree Nil = 0
sTree (Node n t1 t2) = n + (sTree t1) + (sTree t2)

-- Con Monad
sTreem :: (Num a) => Tree a -> Identity a
sTreem Nil = return 0
sTreem (Node n t1 t2) = (return n) >>= \x1 -> 
								sTreem t1 >>= \s1 -> 
								sTreem t2 >>= \s2 -> 
								return (x1 + s1 + s2)

								-}