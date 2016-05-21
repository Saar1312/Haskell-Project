################################################################################
# Laboratorio de Lenguajes de Programacion
# Proyecto 1
#
# Autores:
#	- Isaac Gonzalez 11-10396
#	- Samuel Arleo 10-10969
################################################################################

El proyecto contiene dos archivos principales:

	- Term.hs: Este archivo contiene:

		Definicion de tipos:

			* Term: Tipo recursivo que permite representar las expresiones logicas
			* Equation: Tipo que representa una ecuacion con una equivalencia 
			principal "===".
			* Sust: Tipo que representa las sustituciones simples (x:=y), dobles
			(x,y:=w,z) y triples (x,y,z:=p,q,r)

		Defincion de los operadores logicos:

			* neg: Negacion
			* \/: Disyuncion
			* Conjuncion
			* ==>: Implicacion
			* <==>: Equivalencia (secundaria)
			* !<==>: Inequivalencia
			* ===: Equivalencia (principal) que permite separar en dos terminos un
			teorema.
			* =: Sustitucion

		Especificacion de las precedencias de los operadores:

			8 \/ 
			8 /\ 
			7 ==> 
			6 <==> 
			6 !<==> 
			0 ===

		Definicion de variables desde la "a" a la "z" y las constantes "true" y
		"false"

		Definicion de las "instancias" de show showTerm, showEcu y showSust para 
		elementos de tipo Term, Ecuation y Sust respectivamente.

	- Func.hs: Este archivo contiene funciones que permiten hacer sustituciones 
	en Terminos (tipo Term definido en Term.hs), aplicar instanciacion, regla
	de Leibniz, inferencia, entre otras funciones. Ademas define la clase de tipos
	Sustitucion, que permite realizar sobrecarga sobre la funcion toSust, al
	recibir tuplas de tipo Sust, (Term,Sust,Term) y (Term,Term,Sust,Term,Term) 
	y ejecutar instrucciones especificas.

Y debe contener los archivos:

	- Theorems.hs: Que posee la definicion de los teoremas que permitiran llevar
	a cabo las pruebas. Ejemplo:

------------------------------------------------------
-- Aqui van todos los import que sean necesarios
------------------------------------------------------
prop :: Float -> Equation
prop num
  | num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)     -- axiom
  | num == 3.2  = (p <==> q) === (q <==> p)                   -- axiom
  | num == 3.3  = true === p <==> p                           -- axiom
  | num == 3.4  = p === p <==> true                           -- axiom
  | num == 3.5  = (p <==> q) <==> q === p                     -- theorem
  | num == 3.6  = p === (q <==> q) <==> p                     -- theorem
  | num == 3.7  = (p <==> p) <==> (q <==> q) === true         -- theorem
  | otherwise = error "The statement doesn't exists"

	- TheoremX.hs: Con la demostracion del teorema que se desea comprobar. Posee
	el siguiente formato:

verify = let theorem = enunciado in
	proof theorem
	>>=
	regla1
	>>=
	regla2
	>>=
	.
	.
	>>=
	reglaN
	>>=
	done theorem

Para correr un caso de prueba importar en el archivo con el teorema a probar:

import Func
import Theorems
import Term

Compilar este archivo y correr la funcion verify.