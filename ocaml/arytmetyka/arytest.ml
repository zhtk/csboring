(*

(* Do uprzyjemnienia toplevela: *)

let print_war fmt w = 
  Format.fprintf fmt "<%g, %g>" (min_wartosc w) (max_wartosc w);;

#install_printer print_war;;

*)



let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

let epsilon = 0.0001

let (=.) x y = (x-.epsilon <= y) && (y <= x+.epsilon)


open Arytmetyka;;


Printf.printf "==== Testy obowiazkowe...\n";;

(* testy konstruktory-selektor *)

let x = wartosc_dokladnosc 7. 20.;;
test 31 (not (in_wartosc x 5.));;
test 32 (in_wartosc x 5.7);;
test 33 (in_wartosc x 6.);;
test 34 (in_wartosc x 7.);;
test 35 (in_wartosc x 8.);;
test 36 (in_wartosc x 8.3);;
test 37 (not (in_wartosc x 9.));;

test 39 (min_wartosc x =. 5.6);;
test 40 (max_wartosc x =. 8.4);;
test 41 (sr_wartosc x =. 7.);;


let x = wartosc_od_do 5.6 8.4;;
test 45 (not (in_wartosc x 5.));;
test 46 (in_wartosc x 5.6);;
test 47 (in_wartosc x 6.);;
test 48 (in_wartosc x 7.);;
test 49 (in_wartosc x 8.);;
test 50 (in_wartosc x 8.4);;
test 51 (not (in_wartosc x 9.));;

test 53 (min_wartosc x =. 5.6);;
test 54 (max_wartosc x =. 8.4);;
test 55 (sr_wartosc x =. 7.);;


let x = wartosc_dokladna 7.;;
test 59 (not (in_wartosc x 5.));;
test 60 (not (in_wartosc x 5.6));;
test 61 (not (in_wartosc x 6.));;
test 62 (in_wartosc x 7.);;
test 63 (not (in_wartosc x 8.));;
test 64 (not (in_wartosc x 8.4));;
test 65 (not (in_wartosc x 9.));;

test 67 (min_wartosc x =. 7.);;
test 68 (max_wartosc x =. 7.);;
test 69 (sr_wartosc x =. 7.);;


(* proste operacje *)

let x = wartosc_od_do 2. 3.;;
let y = wartosc_od_do 4. 5.;;
let z = plus x y;;

test 78 (min_wartosc z =. 6.);;
test 79 (sr_wartosc z =. 7.);;
test 80 (max_wartosc z =. 8.);;


let z = razy x y;;

test 85 (min_wartosc z =. 8.);;
test 86 (sr_wartosc z =. 11.5);;
test 87 (max_wartosc z =. 15.);;


let z = minus y x;;

test 92 (min_wartosc z =. 1.);;
test 93 (sr_wartosc z =. 2.);;
test 94 (max_wartosc z =. 3.);;


let z = podzielic y x;;

test 99 (min_wartosc z =. 4./.3.);;
test 100 (sr_wartosc z =. (4./.3.+.5./.2.)/.2.);;
test 101 (max_wartosc z =. 5./.2.);;

(* mnozenie dodatnie / ujemne *)

(* ++ ++ bylo *)

(* -+ ++ *)
let x = wartosc_od_do (-2.) 3.;;
let y = wartosc_od_do 4. 5.;;
let z = razy x y;;
test 111 (min_wartosc z =. -2. *. 5.);;
test 112 (max_wartosc z =. 3. *. 5.);;

(* -+ -+  ->  21,22 *)
let x = wartosc_od_do (-2.) 3.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 118 (min_wartosc z =. -4. *. 3.);;
test 119 (max_wartosc z =. 3. *. 5.);;

(* -+ -+  ->  12,11 *)
let x = wartosc_od_do (-3.) 2.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 125 (min_wartosc z =. -3. *. 5.);;
test 126 (max_wartosc z =. -3. *. -4.);;

(* -+ -+  ->  21,11 *)
let x = wartosc_od_do (-3.) 2.;;
let y = wartosc_od_do (-4.) 1.;;
let z = razy x y;;
test 132 (min_wartosc z =. -4. *. 2.);;
test 133 (max_wartosc z =. -3. *. -4.);;

(* -+ -+  ->  12,22 *)
let x = wartosc_od_do (-3.) 3.;;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 139 (min_wartosc z =. -3. *. 5.);;
test 140 (max_wartosc z =. 3. *. 5.);;


(* -- ++ *)
let x = wartosc_od_do (-3.) (-2.);;
let y = wartosc_od_do 4. 5.;;
let z = razy x y;;
test 147 (min_wartosc z =. -3. *. 5.);;
test 148 (max_wartosc z =. -2. *. 4.);;


(* -- -+ *)
let x = wartosc_od_do (-3.) (-2.);;
let y = wartosc_od_do (-4.) 5.;;
let z = razy x y;;
test 155 (min_wartosc z =. -3. *. 5.);;
test 156 (max_wartosc z =. -3. *. -4.);;


(* -- -- *)
let x = wartosc_od_do (-3.) (-2.);;
let y = wartosc_od_do (-5.) (-4.);;
let z = razy x y;;
test 163 (min_wartosc z =. -2. *. -4.);;
test 164 (max_wartosc z =. -3. *. -5.);;


(* dzielenie dodatnie / ujemne *)

(* ++ ++ było *)

(* ++ -- *)
let x = wartosc_od_do 4. 16.;;
let y = wartosc_od_do (-4.) (-2.);;
let z = podzielic x y;;
test 175 (min_wartosc z =. 16. /. -2.);;
test 176 (max_wartosc z =. 4. /. -4.);;

(* -+ -- *)
let x = wartosc_od_do (-4.) 16.;;
let y = wartosc_od_do (-4.) (-2.);;
let z = podzielic x y;;
test 182 (min_wartosc z =. 16. /. -2.);;
test 183 (max_wartosc z =. -4. /. -2.);;

(* -+ ++ *)
let x = wartosc_od_do (-4.) 16.;;
let y = wartosc_od_do 2. 4.;;
let z = podzielic x y;;
test 189 (min_wartosc z =. -4. /. 2.);;
test 190 (max_wartosc z =. 16. /. 2.);;

(* -- -- *)
let x = wartosc_od_do (-16.) (-4.);;
let y = wartosc_od_do (-4.) (-2.);;
let z = podzielic x y;;
test 196 (min_wartosc z =. -4. /. -4.);;
test 197 (max_wartosc z =. -16. /. -2.);;


Printf.printf "==== Dzielenie przez 0...\n";;

(* dzielenie przez 0 *)


(* ++ ++ *)
let x = wartosc_od_do 1. 2.;;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 209 (min_wartosc z =. 1.);;
test 210 (max_wartosc z =. infinity);;

(* -- ++ *)
let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 216 (min_wartosc z =. neg_infinity);;
test 217 (max_wartosc z =. -1.);;

(* ++ -- *)
let x = wartosc_od_do 1. 2.;;
let y = wartosc_od_do (-1.) 0.;;
let z = podzielic x y;;
test 223 (min_wartosc z =. neg_infinity);;
test 224 (max_wartosc z =. -1.);;

(* -- -- *)
let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do (-1.) 0.;;
let z = podzielic x y;;
test 230 (min_wartosc z =. 1.);;
test 231 (max_wartosc z =. infinity);;


Printf.printf "==== Mnozenie przez nieskonczonosc...\n"

(* mnozenie przez plus nieskonczonosc *)

let x = wartosc_od_do 1. 2.;;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 241 (min_wartosc z =. 1.);;
test 242 (max_wartosc z =. infinity);;

let a = razy z y;;
test 245 (min_wartosc a =. 0.);;
test 246 (max_wartosc a =. infinity);;

let b = wartosc_od_do 2. 3.;;
let a = razy z b;;
test 250 (min_wartosc a =. 2.);;
test 251 (max_wartosc a =. infinity);;

let b = wartosc_od_do (-2.) 3.;;
let a = razy z b;;
test 255 (min_wartosc a =. neg_infinity);;
test 256 (max_wartosc a =. infinity);;

let b = wartosc_od_do (-3.) (-2.);;
let a = razy z b;;
test 260 (min_wartosc a =. neg_infinity);;
test 261 (max_wartosc a =. -2.);;


(* mnozenie przez minus nieskonczonosc *)

let x = wartosc_od_do (-2.) (-1.);;
let y = wartosc_od_do 0. 1.;;
let z = podzielic x y;;
test 269 (min_wartosc z =. neg_infinity);;
test 270 (max_wartosc z =. -1.);;

let a = razy z y;;
test 273 (min_wartosc a =. neg_infinity);;
test 274 (max_wartosc a =. 0.);;

let b = wartosc_od_do 2. 3.;;
let a = razy z b;;
test 278 (min_wartosc a =. neg_infinity);;
test 279 (max_wartosc a =. -2.);;

let b = wartosc_od_do (-2.) 3.;;
let a = razy z b;;
test 283 (min_wartosc a =. neg_infinity);;
test 284 (max_wartosc a =. infinity);;

let b = wartosc_od_do (-3.) (-2.);;
let a = razy z b;;
test 288 (min_wartosc a =. 2.);;
test 289 (max_wartosc a =. infinity);;

Printf.printf "==== Przedziały z dziurami...\n";;

(* dzielenie przez zero *)
let j = wartosc_dokladna 1.;;
let x = wartosc_od_do (-1.) 1.;;


let y = podzielic j x;;
(* y = (-inf,-1) \cup (1,+inf) *)

test 301 (min_wartosc y =. neg_infinity);;
test 302 (max_wartosc y =. infinity);;
test 303 (in_wartosc y (-2.));;
test 304 (in_wartosc y (-1.));;
test 305 (not (in_wartosc y 0.));;
test 306 (in_wartosc y 1.);;
test 307 (in_wartosc y 2.);;
test 308 (in_wartosc y (-1.));;


let a = podzielic j y;;
(* a = (-1,1) *)
test 313 (min_wartosc a =. -1.);;
test 314 (max_wartosc a =. 1.);;
test 315 (in_wartosc a 0.);;


let d = wartosc_od_do 0.25 0.5;;
let z = plus y d;;
(* z = (-inf,-0.5) \cup (1.25,+inf) *)
test 321 (min_wartosc z =. neg_infinity);;
test 322 (max_wartosc z =. infinity);;
test 323 (in_wartosc z (-1.));;
test 324 (not (in_wartosc z (-0.25)));;
test 325 (not (in_wartosc z 0.));;
test 326 (not (in_wartosc z 1.));;
test 327 (in_wartosc z 1.5);;

let e = podzielic j z;;
(* e = (-2,0.8) *)
test 331 (min_wartosc e =. -2.);;
test 332 (max_wartosc e =. 0.8);;
test 333 (not (in_wartosc e (-3.)));;
test 334 (in_wartosc e (-1.));;
test 335 (in_wartosc e 0.);;
test 336 (in_wartosc e 0.5);;
test 337 (not (in_wartosc e 1.));;

(* Jeszcze mozna by zrobic dzielenie czegos skonczonego przez
   nieskonczonosc - z roznie dobranymi znakami *)

(* No i dzielenie nieskonczonosci przez nieskonczonosc *)
let j = wartosc_dokladna 1.;;
let x = wartosc_od_do 0. 1.;;
let z = podzielic j x;;
(* z = (1,+inf) *)
test 347 (min_wartosc z =. 1.);;
test 348 (max_wartosc z =. infinity);;

let a = podzielic z z;;
(* a = (0,+inf) *)
test 352 (min_wartosc a =. 0.);;
test 353 (max_wartosc a =. infinity);;

(* Test Patryka Czajki *)

let a = wartosc_od_do (-.1.) 0.1;;
let b = wartosc_od_do 1. 10.;;
let jeden = wartosc_dokladna 1.;;
let a = podzielic jeden a;; (* a = dopełnienie (-1., 10.) *)

let c = razy b a;;

test 361 (in_wartosc c (-1.));;
test 362 (in_wartosc c 10.);;
test 363 (not (in_wartosc c 0.));;
test 364 (not (in_wartosc c (-0.9)));;
test 365 (not (in_wartosc c 9.9));;

test 371 (in_wartosc c (-1.1));;
test 372 (in_wartosc c 10.1);;

test 381 (min_wartosc c =. neg_infinity);;
test 382 (max_wartosc c =. infinity);;

(* .... i pewnie sporo innych przykladow ... *)

let _ = 
  if !zle = 0 then 
    Printf.printf "\nTesty OK!\n"
  else  
    Printf.printf "\nBlednych testow: %d...\n" !zle
;;

(* emacs:
do przerabiania outputu toplevela na test 2y
M-x query-replace-regexp
# \(.*\);;[     ]*
.* =[   
]*\(.*\)
na
test 8 (\1 = \2);;
*)

(*
*** Local Variables:
*** inferior-caml-program: "ocaml -I _build arytmetyka.cmo"
*** End:
*)
