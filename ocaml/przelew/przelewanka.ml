exception Found of int

module SS = Set.Make ( 
                struct
                        type t = int array
                        let compare = compare
                end
        )

(* Funkcja do przelewania
 * Jeśli z = -1 to wodę bierzemy z kranu 
 * jeśli w = -1 to wylewamy do zlewu 
 * Stan musi być skopiowany przed użyciem! *)
let przelej stan limits z w =        
        if z = -1 && w = -1 then ()
        else if z = -1 then 
                stan.(w) <- limits.(w)
        else if w = -1 then
                stan.(z) <- 0
        else
                let delta = ref 0 in
                delta := limits.(w) - stan.(w) ;
                delta := min !delta stan.(z) ;
                stan.(z) <- stan.(z) - !delta ;
                stan.(w) <- stan.(w) + !delta 

let przelewanka arr limits = 
        let n = Array.length arr in
        let s = ref SS.empty in (* Set na sprawdzone stany *)
        let q = Queue.create () in (* Kolejka dla BFS *)

        (* Do kolejki dodaję stan do sprawdzenia i ilość ruchów *)
        Queue.push ((Array.make n 0),0) q ;
        
        (* Główna pętla *)
        while not (Queue.is_empty q) do
                let (a,num) = Queue.pop q in
                
                if compare a arr = 0 then raise (Found num);

                if not (SS.mem a !s) then (
                        s := SS.add a !s ;
                        
                        (* Iteracja po: *)
                        for z = -1 to n-1 do (* źródle *)
                        for w = -1 to n-1 do (* celu *)
                                let tmp = Array.copy a in
                                przelej tmp limits z w ;
                                Queue.push (tmp, (num+1)) q
                        done done
                )       
        done ;
        -1

let przelewanka a = 
        let arr = Array.map snd a in
        let lim = Array.map fst a in 

        try przelewanka arr lim
        with Found x -> x
