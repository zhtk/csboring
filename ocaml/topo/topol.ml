exception Cykliczne

let isCycle l = 
        (* Mapa przetwarzania *)
        (* 0 - nieodwiedzony 
         * 1 - przetwarzany
         * 2 - przetworzony *)
        let vis = ref PMap.empty in
        (* Tworzymy mapę: element -> wierzchołki na jakie wskazuje *)
        let vert = ref PMap.empty in
        List.fold_left (fun a (v,n) -> vert := PMap.add v n !vert ) () l ;
        (* DFS *)
        let rec dfs v =
                (if not (PMap.mem v !vis) then vis := PMap.add v 0 !vis);
                let col = PMap.find v !vis in

                if col = 1 then raise Cykliczne
                else if col = 2 then ()
                else (
                        vis := PMap.add v 1 !vis ;
                        List.fold_left (fun acc v -> dfs v) () (if PMap.mem v !vert then PMap.find v !vert else []) ;
                        vis := PMap.add v 2 !vis
                )
        in List.fold_left (fun a v -> dfs (fst v)) () l

let topol l =
        isCycle l;
        (* Tworzymy mapę: element -> wierzchołki na jakie wskazuje *)
        let vert = ref PMap.empty in
        List.fold_left (fun a (v,n) -> vert := PMap.add v n !vert) () l ;
        (* Mapa odwiedzin *)
        let vis = ref PMap.empty in
        (* DFS *)
        let rec dfs (v, nodes) acc =
                if PMap.mem v !vis then acc
                else (
                        vis := PMap.add v true !vis ;
                        v::(
                                List.fold_left 
                                (
                                        fun acc v -> 
                                                let nodes = ( if PMap.mem v !vert then PMap.find v !vert else [] ) in 
                                                dfs (v,nodes) acc
                                ) 
                                acc nodes
                        )
                )
        in
        (* Wynik *)
        List.fold_left (fun a v -> dfs v a) [] l
