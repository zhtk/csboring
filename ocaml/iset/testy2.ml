(* w przypadku tych samych numerkow, pisalem testy recznie i nie chce mi sie *)
(* ich pozniej dorzucac *)

let zle = ref 0;;
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end;;

open List;;
open ISet;;
open Printf;;

let first  (a,_,_) = a;;
let second (_,b,_) = b;;
let third  (_,_,c) = c;;

Printf.printf "==== STARTO!...\n";;
let a = ref empty;;
test 0 (elements !a = []);;
test 1 (is_empty !a);;
a := add (0,0) !a;;
test 2 (not (is_empty !a));;
a := remove (0,0) !a;;
test 3 (is_empty !a);;
a := add (1,1) !a;;
a := add (2,2) !a;;
test 4 (is_empty (first (split 1 !a)));;
test 5 (second (split 1 !a));;
test 6 (not (second (split 3 !a)));;
test 7 (is_empty (third (split 3 !a)));;
a := add (3,3) !a;;
test 8 (mem 1 !a);;
test 9 (mem 2 !a);;
test 10 (mem 3 !a);;
test 11 (not (mem 4 !a));;
test 12 (not (mem 0 !a));;
test 13 (below 42 !a = 3);;
let b = ref !a;;
b := remove (2,2) !a;;
b := add (1,3) !b;;
test 14 (!b = !a);;
test 15 (length (elements !b) = 1);;
b := remove (2,2) !b;;
test 16 (length (elements !b) = 2);;
test 17 (length (elements (first (split 2 !b))) = 1);;
test 18 (length (elements (third (split 2 !b))) = 1);;
test 19 (not (second (split 2 !b)));;
b := remove (min_int, max_int) !b;;
test 20 (is_empty !b);;

(* testy na below na drzewie z jednym wezlem *)
b := add (min_int, max_int) !b;;
test 21 (length (rev (fold (fun s acc -> s :: acc) !b [])) = 1);;
test 22 (below max_int !b = max_int);;
test 23 (below (max_int-1) !b = max_int);;
test 24 (below 0 !b = max_int);;
test 25 (below (max_int/2) !b = max_int);;
test 26 (below (-2) !b = max_int);;
test 27 (below (-3) !b = max_int - 1);;
test 28 (below (-4) !b = max_int - 2);;
test 29 (below min_int !b = 1);;
test 30 (below (min_int/2) !b = (max_int/2)+2);;
test 31 (below (min_int+1) !b = 2);;
test 32 (is_empty (first (split min_int !b)));;
let tmp = first (split (max_int/2) (third (split (min_int/2) !b)));;
test 33 (snd (hd (elements tmp)) = max_int/2 - 1);;
test 34 (below max_int tmp = max_int - 1);;
test 35 (below ((max_int/2)-2) tmp = max_int - 2);;
let tmp = third (split 0 tmp);;
test 36 (below 0 tmp = 0);;
test 37 (below 1 tmp = 1);;
test 38 (below (max_int/4) tmp = max_int/4);;
test 39 (below max_int tmp = (max_int/2) - 1);;
test 40 (below min_int tmp = 0);;

(* testy na elements *)
a := remove (min_int, max_int) !a;;
a := add (min_int,min_int) !a;;
let aux x = length (elements x);;
test 41 (aux !a = 1);;
a := add (min_int+2,min_int+2) !a;;
test 42 (aux !a = 2);;
a := add (min_int+1,min_int+1) !a;;
test 43 (aux !a = 1);;
a := add (max_int, max_int) !a;;
test 43 (aux !a = 2);;
a := add (0,0) !a;;
test 43 (aux !a = 3);;
a := add (-42,42) !a;;
test 44 (aux !a = 3);;
a := add (-44,-44) !a;;
a := add (44, 44) !a;;
test 45 (aux !a = 5);;
a := add (43, 43) !a;;
test 46 (aux !a = 4);;
a := add (-43, -43) !a;;
test 47 (aux !a = 3);;
a := add (min_int, 0) !a;;
test 47 (aux !a = 2);;
a := add (46, max_int) !a;;
test 48 (aux !a = 2);;
a := remove (48, 48) !a;;
test 49 (aux !a = 3);;
a := remove (min_int, 47) !a;;
test 50 (aux !a = 1);;
a := remove (50, max_int) !a;;
test 51 (aux !a = 1);;
a := remove (49, 49) !a;;
test 52 (aux !a = 0);;

(* testy na dodawanie pojedynczych w prawa strone *)
let a = ref empty;;
test 60 (elements !a = []);;
a := add (1,1) !a;;
test 61 (below max_int !a = 1);;
test 61 (elements !a = [(1,1)]);;
a := add (2,2) !a;;
test 62 (below max_int !a = 2);;
test 62 (elements !a = [(1,2)]);;
a := add (3,3) !a;;
test 63 (below max_int !a = 3);;
test 63 (elements !a = [(1,3)]);;
a := add (4,4) !a;;
test 64 (below max_int !a = 4);;
test 64 (elements !a = [(1,4)]);;
a := add (5,5) !a;;
test 65 (below max_int !a = 5);;
test 65 (elements !a = [(1,5)]);;

(* w lewa strone *)
a := remove (2,5) !a;;
test 66 (below max_int !a = 1);;
test 66 (elements !a = [(1,1)]);;
a := add (0,0) !a;;
test 67 (below max_int !a = 2);;
test 67 (elements !a = [(0,1)]);;
a := add (-1,-1) !a;;
test 68 (below max_int !a = 3);;
test 68 (elements !a = [(-1,1)]);;
a := add (-2,-2) !a;;
test 69 (below max_int !a = 4);;
test 69 (elements !a = [(-2,1)]);;
a := add (-3,-3) !a;;
test 70 (below max_int !a = 5);;
test 70 (elements !a = [(-3,1)]);;
a := add (-4,-4) !a;;
test 71 (below max_int !a = 6);;
test 71 (elements !a = [(-4,1)]);;
a := add (-5,-5) !a;;
test 72 (below max_int !a = 7);;
test 72 (elements !a = [(-5,1)]);;

a := remove (-2, -2) !a;;
test 73 (below max_int !a = 6);;
test 73 (elements !a = [(-5,-3);(-1,1)]);;
a := remove (-4, -4) !a;;
test 74 (below max_int !a = 5);;
test 74 (elements !a = [(-5,-5);(-3,-3);(-1,1)]);;
a := remove (-5, -5) !a;;
test 75 (below max_int !a = 4);;
test 75 (elements !a = [(-3,-3);(-1,1)]);;
a := remove (-3, -3) !a;;
test 76 (below max_int !a = 3);;
test 76 (elements !a = [(-1,1)]);;
a := remove (-1, -1) !a;
test 77 (below max_int !a = 2);;
test 77 (elements !a = [(0,1)]);;
a := remove (0, 0) !a;;
test 78 (below max_int !a = 1);;
test 78 (elements !a = [(1,1)]);;
a := remove (1, 1) !a;;
test 79 (below max_int !a = 0);;
test 79 (elements !a = []);;

(* testy na przypadki graniczne *)
a := empty;;
test 80 (is_empty !a);;
a := add (min_int, min_int) !a;;
test 81 (elements !a = [(min_int,min_int)]);;
a := add (min_int+2,min_int+2) !a;;
test 82 (elements !a = [(min_int,min_int);(min_int+2,min_int+2)]);;
a := add (min_int+1,min_int+1) !a;;
test 83 (elements !a = [(min_int,min_int+2)]);;

a := remove (min_int+2, min_int+2) !a;;
test 84 (elements !a = [(min_int,min_int+1)]);;
a := remove (min_int, min_int) !a;;
test 85 (elements !a = [(min_int+1,min_int+1)]);;
a := remove (min_int+1, min_int+1) !a;;
test 86 (elements !a = []);;
a := add (min_int+2,min_int+2) !a;;
test 87 (elements !a = [(min_int+2,min_int+2)]);;
a := add (min_int, min_int) !a;;
test 88 (elements !a = [(min_int,min_int);(min_int+2,min_int+2)]);;
a := add (min_int+1,min_int+1) !a;;
test 89 (elements !a = [(min_int,min_int+2)]);;

a := remove (min_int+1, min_int+1) !a;;
test 90 (elements !a = [(min_int,min_int);(min_int+2,min_int+2)]);;
a := remove (min_int, min_int) !a;;
test 91 (elements !a = [(min_int+2,min_int+2)]);;
a := remove (min_int+2, min_int+2) !a;;
test 92 (elements !a = []);;
a := add (max_int, max_int) !a;;
test 93 (elements !a = [(max_int,max_int)]);;
a := add (max_int-2, max_int-2) !a;;
test 94 (elements !a = [(max_int-2,max_int-2);(max_int,max_int)]);;
a := add (max_int-1, max_int-1) !a;;
test 95 (elements !a = [(max_int-2,max_int)]);;

a := remove (max_int-2, max_int-2) !a;;
test 96 (elements !a = [(max_int-1,max_int)]);;
a := remove (max_int, max_int) !a;;
test 97 (elements !a = [(max_int-1,max_int-1)]);;
a := remove (max_int-1, max_int-1) !a;;
test 98 (elements !a = []);;
a := add (max_int-2, max_int-2) !a;;
test 99 (elements !a = [(max_int-2,max_int-2)]);;
a := add (max_int, max_int) !a;;
test 100 (elements !a = [(max_int-2,max_int-2);(max_int,max_int)]);;
a := add (max_int-1, max_int-1) !a;;
test 101 (elements !a = [(max_int-2,max_int)]);;

a := remove (max_int-1, max_int-1) !a;;
test 102 (elements !a = [(max_int-2,max_int-2);(max_int,max_int)]);;
a := remove (max_int, max_int) !a;;
test 103 (elements !a = [(max_int-2,max_int-2)]);;
a := remove (max_int-2, max_int-2) !a;;
test 104 (elements !a = []);;
test 105 (is_empty !a);;

(* testy na fold *)
let fun1 t = fold (fun (a,b) acc -> a+b+acc) t 0;;
test 120 (fun1 !a = 0);;
a := add (5,5) !a;;
test 121 (fun1 !a = 10);;
a := add (0, max_int) !a;;
test 121 (fun1 !a = max_int);;
a := remove (0, max_int) !a;;
a := add (0,0) !a;;
test 122 (fun1 !a = 0);;
a := add (2,2) !a;;
test 123 (fun1 !a = 4);;
a := add (4,4) !a;;
test 124 (fun1 !a = 12);;
a := add (6,6) !a;;
test 125 (fun1 !a = 24);;
a := add (5,5) !a;;
test 126 (fun1 !a = 14);;
a := add (1,1) !a;;
test 127 (fun1 !a = 12);;
a := add (3,3) !a;;
test 128 (fun1 !a = 6);;

let fun2 t = fold (fun (a,b) acc -> acc+(a-b)) t 0;;
test 130 (fun2 !a = -6);;
a := remove (3,3) !a;;
test 131 (fun2 !a = -4);;
a := remove (1,1) !a;;
test 132 (fun2 !a = -2);;
a := remove (5,5) !a;;
test 133 (fun2 !a = 0);;
a := add (1,1) !a;;
test 134 (fun2 !a = -2);;
a := remove (0,10) !a;;
test 135 (fun2 !a = 0);;
a := add (min_int/2, (max_int/2)-100) !a;;
test 136 (fun2 !a = min_int + 101);;
a := remove (min_int/4, max_int/4) !a;;
test 137 (fun2 !a = (min_int/2)+102);;
a := remove ((min_int/2+100000), (min_int/3)) !a;;
test 138 (fun2 !a = (min_int/3) - 99896);;
a := remove (min_int,max_int) !a;;
test 139 (fun2 !a = 0);;

let fun3 t = fold (fun (a,_) acc -> a :: acc) t [];;
let fun4 t = fold (fun (_,a) acc -> a :: acc) t [];;
test 140 (is_empty !a);;
a := add (42,42) !a;;
test 141 (fun3 !a = [42]);;
test 142 (fun4 !a = [42]);;
a := add (40,40) !a;;
test 143 (fun3 !a = [42;40]);;
test 144 (fun4 !a = [42;40]);;
a := add (44,44) !a;;
test 145 (fun3 !a = [44;42;40]);;
test 146 (fun4 !a = [44;42;40]);;
a := add (max_int,max_int) !a;;
test 147 (fun3 !a = [max_int;44;42;40]);;
test 148 (fun4 !a = [max_int;44;42;40]);;
a := add (43,43) !a;;
test 149 (fun3 !a = [max_int;42;40]);;
test 150 (fun4 !a = [max_int;44;40]);;
a := add (41,41) !a;;
test 151 (fun3 !a = [max_int;40]);;
test 152 (fun4 !a = [max_int;44]);;
a := add (min_int,0) !a;;
test 152 (fun3 !a = [max_int;40;min_int]);;
test 153 (fun4 !a = [max_int;44;0]);;
a := add (min_int,max_int) !a;;
test 154 (fun3 !a = [min_int]);;
test 155 (fun4 !a = [max_int]);;


let q = split 42 empty;;
test 156 (is_empty (first q) && not (second q) && is_empty (third q));;


Printf.printf "==== ENDOOO... \n";;

let _ = 
  if !zle = 0 then 
    Printf.printf "\nTesty OK!\n"
  else  
    Printf.printf "\nBlednych testow: %d...\n" !zle
;;