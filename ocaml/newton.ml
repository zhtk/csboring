(* Metoda newtona na sqrt *)
(* NIE DZIAŁA *)

let newton x = 
	let epsilon = 0.001
	
	let good p = 
		if (x-.p*.p)*.(x-.p*.p) > epsilon*.epsilon then false
		else true
	
	let improve p = (p/.x +. p)/.2.0
	
	let pom p =
		if isEnough p then p
		else pom (improve p)

	pom 1
;;
