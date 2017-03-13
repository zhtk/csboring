(* Funkcje matematyczne *)

(* algorytm euklidesa *)

let rec euklides a b = 
	if a>0 && b>0 then 
		if a>b then (euklides (a-b) b)
		else (euklides a (b-a))
	else
		if a>0 then a
		else b

(* implementacja wykładnicza *)

let rec fib1 n =
	if n=0 then 1
	else if n=1 then 1
	else (fib1 (n-1))+(fib1 (n-2))

(* lepsiejsza *)

let fib2 n =
	let rec fibpom a b n =
		if n=0 then a
		else fibpom b (a+b) (n-1)
	in fibpom 0 1 n

(* Odwracanie kolejności cyfr w liczbie *)
(* 123 -> 321 *)

let reverse n =
	let rec pom wynik liczba =
		if liczba = 0 then wynik
		else pom (wynik*10 + (liczba mod 10)) (liczba/10)
	in pom 0 n

(* Silnia *)
let rec silnia n = 
	if n<2 then n
	else n*silnia (n-1)
