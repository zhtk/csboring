type wielomian = float list

let oblicz wl x =
	List.fold_left 
	(fun a wsp -> a*.x +. wsp)
	0.
	wl

let stopien w =
	List.length w-1

let rec rozszerz w len = 
	if len = 0 then w
	else rozszerz ((0.)::w) (len-1)

let suma w1 w2 =
	let rec ubersuma wx wy =
		if List.tl wx = [] then [ (List.hd wx) +. (List.hd wy)] 
		else ((List.hd wx) +. (List.hd wy))::(ubersuma (List.tl wx) (List.tl wy))
	in 
	let delta = (List.length w1) - (List.length w2) in
	if delta = 0 then ubersuma w1 w2 
	else if delta>0 then ubersuma w1 (rozszerz w2 delta) 
	else ubersuma w2 (rozszerz w1 (0-delta)) 

let pochodna w1 w2 = 
	let rec uber wx wy =
		if List.tl wx = [] then [ (List.hd wx) +. (List.hd wy)] 
		else ((List.hd wx) +. (List.hd wy))::(uber (List.tl wx) (List.tl wy))
	in 
	let delta = (List.length w1) - (List.length w2) in
	if delta = 0 then uber w1 w2 
	else if delta>0 then uber w1 (rozszerz w2 delta) 
	else uber w2 (rozszerz w1 (0-delta)) 
