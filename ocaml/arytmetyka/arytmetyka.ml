(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)

(* Typ reprezentujący niedokładne wartości. *)
type wartosc = (float * float) list
(* Implicite zakładamy, że wszystkie argumenty typu float są liczbami *)
(* rzeczywistymi, tzn. są różne od infinity, neg_infinity i nan.      *)


(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
(* val wartosc_dokladnosc: float -> float -> wartosc *)   
let wartosc_dokladnosc s e =
	[ ( s-.(e*.s/.100.) , s+. (e*.s/.100.) ) ]

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
(* val wartosc_od_do: float -> float -> wartosc *)                           
let wartosc_od_do p k =
	[ (p,k) ]


(* wartosc_dokladna x = [x;x]        *)
(* val wartosc_dokladna: float -> wartosc *)  
let wartosc_dokladna x = 
	[ (x,x) ]

(* in_wartosc w x = x \in w *)
(* val in_wartosc: wartosc -> float -> bool *) 
let in_wartosc wart c =
	List.fold_left 
	(
		fun a h -> (a || ( (fst h)<= c) && (c<=(snd h)) )
	) 
	false wart

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
(* val min_wartosc: wartosc -> float *)      

let min_wartosc wart =
	List.fold_left (fun a h -> min a (fst h)) (fst (List.hd wart)) wart

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
(* val max_wartosc: wartosc -> float *)      
let max_wartosc wart =
	List.fold_left (fun a h -> max a (snd h)) (snd (List.hd wart)) wart

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
(* val sr_wartosc:  wartosc -> float *)      

let sr_wartosc w =
	(max_wartosc w +. min_wartosc w) /. 2.0
    
(* Operacje arytmetyczne na niedokładnych wartościach. *)
(* val plus:      wartosc -> wartosc -> wartosc *) 
(* val minus:     wartosc -> wartosc -> wartosc *)
(* val razy:      wartosc -> wartosc -> wartosc *)
(* val podzielic: wartosc -> wartosc -> wartosc *)                           

let plus w1 w2 = 
	List.fold_left
	(
		fun a h -> 
		((fst h)+.min_wartosc w2, (snd h)+.max_wartosc w2)::a
	)
	[]
	w1

let minus w1 w2 = 
	List.fold_left
	(
		fun a h -> 
		((fst h)-.max_wartosc w2, (snd h)-.min_wartosc w2)::a
	)
	[]
	w1

let razy w1 w2 =
	List.fold_left
        (
                fun a v1 ->
                List.fold_left
                (
                        fun a v2 ->
                        (
                        min ((fst v1)*.(fst v2)) (
                        min ((fst v1)*.(snd v2)) (
                        min ((snd v1)*.(fst v2)) (      
                            ((snd v1)*.(snd v2)) ))),
                        max ((fst v1)*.(fst v2)) ( 
                        max ((fst v1)*.(snd v2)) (
                        max ((snd v1)*.(fst v2)) (      
                            ((snd v1)*.(snd v2)) ))) 
                        )::a
                )
                a
                w2
        )
        [] 
        w1

let podzielic w1 w2 = 
        List.fold_left
        (
                fun a v1 ->
                List.fold_left
                (
                        fun a v2 ->
                        if in_wartosc [v2] 0. then 
                        (
                         min ((fst v1)/.(fst v2)) (
                         min ((fst v1)/.(  0.  )) (
                         min ((snd v1)/.(fst v2)) (      
                             ((snd v1)/.(  0.  )) ))),
                         max ((fst v1)/.(fst v2)) ( 
                         max ((fst v1)/.(  0.  )) (
                         max ((snd v1)/.(fst v2)) (      
                             ((snd v1)/.(  0.  )) )))       
                        )::((
                         min ((fst v1)/.(  0.  )) (
                         min ((fst v1)/.(snd v2)) (
                         min ((snd v1)/.(  0.  )) (      
                             ((snd v1)/.(snd v2)) ))),
                         max ((fst v1)/.(  0.  )) ( 
                         max ((fst v1)/.(snd v2)) (
                         max ((snd v1)/.(  0.  )) (      
                             ((snd v1)/.(snd v2)) )))
                        )::a)
                        else 
                        (
                        min ((fst v1)/.(fst v2)) (
                        min ((fst v1)/.(snd v2)) (
                        min ((snd v1)/.(fst v2)) (      
                            ((snd v1)/.(snd v2)) ))),
                        max ((fst v1)/.(fst v2)) ( 
                        max ((fst v1)/.(snd v2)) (
                        max ((snd v1)/.(fst v2)) (      
                            ((snd v1)/.(snd v2)) ))) 
                        )::a                           
                )
                a
                w2
        )
        [] 
        w1

	
