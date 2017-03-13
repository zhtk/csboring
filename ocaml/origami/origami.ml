type point = float * float
type kartka = point -> int

let prostokat (ax, ay :point) (bx, by :point) = 
        fun (x, y :point) -> 
                if x>=ax && x<=bx && y>=ay && y<=by then 1
                else 0

let kolko (px, py :point) r =
        fun (x, y :point) ->
                if (x-.px)*.(x-.px) +. (y-.py)*.(y-.py) <= r*.r then 1
                else 0

(* Iloczyn wektorowy *)
let det (p1x,p1y :point) (p2x,p2y :point) (p3x,p3y :point) = 
        let p2x,p2y = (p2x-.p1x),(p2y-.p1y) in
        let p3x,p3y = (p3x-.p1x),(p3y-.p1y) in
        p2x *. p3y -. p2y *. p3x

let sq x =
        x *. x

let symetria (ax, ay) (bx, by) (px, py) = 
        (* Magiczny wzór z algorytm.org *)
        let u = (px -. ax) *. (bx -. ax) +. (py -. ay) *. (by -. ay) in
        let u = u /. ((sq (ax -. bx)) +. (sq (ay -. by))) in
        (* Rzut p na prostą a-b = r *)
        let rx = ax +. u *. (bx -. ax) in
        let ry = ay +. u *. (by -. ay) in
        (* Wektor r -> p *)
        let rpx = px -. rx and rpy = py -. ry in
        (* Wreszcie wynik *)
        rx -. rpx, ry -. rpy


let zloz (ax, ay :point) (bx, by :point) (k :kartka) =
        fun (x, y :point) ->
                let iw = det (ax, ay) (bx, by) (x, y) in
                if iw = 0.0 then 
                        k (x, y)
                else if iw < 0.0 then
                        0
                else
                        k (x, y) + k (symetria (ax,ay) (bx, by) (x,y))

let skladaj (l :(point*point) list) (k :kartka) =
        List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) k l
