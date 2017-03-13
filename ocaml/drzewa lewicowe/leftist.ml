type 'a queue = Pusty | Node of 'a * 'a queue * 'a queue * int

let empty = Pusty

let is_empty x = 
        match x with
        | Pusty -> true 
        | _ -> false

exception Empty

let rec join t1 t2 =
        if t1 = Pusty then t2 else
        if t2 = Pusty then t1 else
        (* W korzeniu t1 ma się znajdować najmniejszy priorytet*)
        let Node(v1,_,_,_) = t1 and
            Node(v2,_,_,_) = t2 in
        let t1 = (if v1< v2 then t1 else t2) and
            t2 = (if v1>=v2 then t1 else t2) in
        (* Złączanie poddrzew *)
        let Node(v1, l1, r1, h1) = t1 in
        let merged = (join r1 t2) in
        if l1 = Pusty then
                Node (v1, merged, Pusty, 1)
        else
        let Node(_, _, _, hm) = merged and
            Node(_, _, _, h2) = l1 in
        Node(v1,
             (if hm >= h2 then merged else l1),
             (if hm <  h2 then merged else l1),
             (min hm h2) + 1
            )

let add a t =
        join t (Node (a, Pusty, Pusty, 1))

let delete_min t = 
        match t with
        | Pusty -> raise Empty
        | Node ( w, t1, t2, _) -> (w, join t1 t2)
