(* W bólach ale w końcu wyłapałem lietrówki 
 *
 * ISet
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Czyli wg. licencji powinienem ten kod publicznie udostępnić? *)

(* Działania na przedziałach intów *)

module R = struct 

type range = int * int

let size (a,b) =
        let s = b-a in 
        if s<0 || s+1<0 then max_int 
        else s+1

let is_mergable (a,b) (c,d) = 
        (a<= c && c<=b) || (a<= d && d<=b) || 
        (c<= a && a<=d) || (c<= b && b<=d) (* ||
        (if b=max_int then false else b+1=c) ||
        (if d=max_int then false else d+1=a) *)

let merge (a,b) (c,d) = 
        let l = List.sort compare [a;b;c;d] in
        (List.nth l 0),(List.nth l 3)

let is_in v (a,b) =
        a<=v && v<=b

let compare (a,b) (c,d) =
        if a=c && b=d then 0
        else if b<c then -1
        else 1

end
(* Set *)

let cmp = R.compare

type set =
  | Empty
  | Node of set * R.range * set * int * int (* Ilość liczb w drzewie *)

type t = set

let numamt = function
        | Empty -> 0
        | Node(_,_,_,_,x) -> x

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let make l k r = Node (l, k, r, max (height l) (height r) + 1, 
                ((numamt l)+(numamt r)+(R.size k))
        )

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, ((numamt l)+(numamt r)+(R.size k)))

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let create cmp = Empty
let empty = Empty

let is_empty x = 
  x = Empty

let rec add_one cmp x = function
  | Node (l, k, r, h, _) ->
      let c = cmp x k in
      if c = 0 then make l x r
      else if c < 0 then
        let nl = add_one cmp x l in
        bal nl k r
      else
        let nr = add_one cmp x r in
        bal l k nr
  | Empty -> make Empty x Empty

let rec join cmp l v r =
  match (l, r) with
    (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, lh,_), Node(rl, rv, rr, rh,_)) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r

(* Funkcje szukające najmniejszego lewego i prawego przedziału*)
let rec lewy (a,b) = function
        | Empty -> failwith "nicsieniestalo"
        | Node(l, x, r, _,_) -> 
                let (c,d) = x in
                if (c<=a && a<=d) || (if d=max_int then false else d+1=a) then x
                else 
                        if c>a then
                                try lewy (a,b) l
                                with ex -> 
                                        if b>=c then x
                                        else raise ex
                        else lewy (a,b) r

let rec prawy (a,b) = function
        | Empty -> failwith "nicsieniestalo"
        | Node(l, x, r, _,_) -> 
                let (c,d) = x in
                if (c<=b && b<=d) || (if b=max_int then false else b+1=c) then x
                else 
                        if d<b then
                                try prawy (a,b) r
                                with ex -> 
                                        if a<=d then x
                                        else raise ex
                        else prawy (a,b) l

let lpget r s = 
        ((try lewy r s with ex->r),
         (try prawy r s with ex->r))

(* *** *)

let split x set =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _,_) ->
        let c = cmp x v in
        if c = 0 then (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl v r)
        else
          let (lr, pres, rr) = loop x r in (join cmp l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
   (setl, pres, setr)

let add ((a,b) as r) set = 
          let (lwy,pwy) = lpget (a,b) set in
          if lwy = r && pwy = r then add_one cmp r set
          else
                  let (la,_,ra) = split lwy set in
                  let (_,_,ra) = split pwy ra in 
                  let nr = (min (fst lwy) a, max (snd pwy) b) in
                  join cmp la nr ra               

let remove ((a,b) as x) set =
        let (lw,pw) = lpget x set in
        (* Usuwamy przedział a-b *)
        let (l,_,p) = split lw set in
        let (_,_,p) = split pw p in
        (* Sklejamy resztę w nowy set *)
        let set = (
                match (l,p) with
                | (Empty, _) -> p
                | (_,Empty) -> l
                | (_,_) ->
                        let m = min_elt p in
                        let s = remove_min_elt p in
                        join cmp l m s
        ) in 
        (* dorzucamy elementy przedziału które były usunięte
         * ale nie powinny być *)
        let set = (if fst lw < a then add ((fst lw), a-1) set else set) in
        let set = (if snd pw > b then add (b+1, snd pw) set else set) in
        (* Wynik *)
        set

let mem x set =
  let rec loop = function
    | Node (l, ((a,b) as k), r, _, _) ->
        R.is_in x k  || loop (if x<a then l else r)
    | Empty -> false in
  loop set

let exists = mem

let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _,_) -> 
        loop l; f k; loop r in
  loop set

let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _,_) ->
          loop (f k (loop acc l)) r in
  loop acc set

let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _,_) -> 
        loop (k :: loop acc r) l in
  loop [] set

let sm a b = 
        if a+b<0 then max_int else a+b

let rec below n = function
        | Empty -> 0
        | Node(l,r,p,_,s) ->
                if snd r < n then sm (sm (R.size r) (numamt l)) (below n p) else
                if n < fst r then below n l else
                (* n trafia w przedzial *)
                sm (R.size ((fst r), n)) (numamt l)

let split x set = 
        let (lw, pw) = lpget (x,x) set in
        let (l, _, p) = split lw set in
        let (_, _, p) = split pw p in
        let l = (if fst lw < x then add (fst lw, x - 1) l else l) in
        let p = (if snd pw > x then add (x+1, snd pw) p else p) in
        (l, (mem x set), p)
