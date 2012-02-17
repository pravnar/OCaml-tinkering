let map (f: 'a-> 'b) (lst: 'a list) : 'b list = 
List.fold_right (fun x y -> (f x)::y) lst []

let filter (f: 'a -> bool) (lst: 'a list) : 'a list = 
List.fold_right (fun x y -> if f x then x::y else y) lst []

let length (lst: 'a list) : int = 
List.fold_right (fun _ x -> x + 1) lst 0

let reverse (lst : 'a list) : 'a list =
List.fold_left (fun x y -> y::x) [] lst

let nth (n: int) (lst: 'a list) : 'a = 
  let f ((i:int), (x:'a)) (y:'a) : int*'a = 
    if i<0 then (i,x)
    else if i=0 then (i-1, y) 
      else (i-1,x)
  in
    if n>=List.length lst || n<0 then failwith "Index negative or out of bounds"
    else snd (List.fold_left f (n, List.hd lst) lst)

let copy (lst: 'a list) =
List.fold_right (fun x y -> x::y) lst []

let flatten (lst: 'a list list) : 'a list = 
List.fold_left (fun x y -> x@y) [] lst

let unzip (lst: ('a*'b) list) : 'a list * 'b list = 
  match lst with 
    | [] -> ([],[])
    | _ -> ((List.fold_right (fun x y -> (fst x)::y) lst []), (List.fold_right (fun x y -> (snd x)::y) lst []))

let forall (p: 'a->bool) (lst: 'a list) : bool = 
  let falsecount = List.fold_left (fun x y -> if p y then x else x+1) 0 lst
  in
    falsecount=0 

let exists (p: 'a->bool) (lst : 'a list) : bool = 
(List.fold_left (fun x y -> if p y then x+1 else x) 0 lst) >= 1 

let max (o : 'a->'a->bool) (lst : 'a list) : 'a = 
  if lst=[] then None
  else List.fold_left (fun x y -> if o x y then x else y) (List.hd lst) lst

let powerset (lst: 'a list) : 'a list list =
  let ps1 l =
    List.fold_left (fun a x -> (List.filter (fun y -> y!=x) l) :: a) [] l
  in
  let f (i,l1) x =
    if i<=0 then (i,l1)
    else if i= List.length lst then (i-1, ps1 lst)
    else (i-1, (List.fold_left (fun x y -> (ps1 y)@x) l1 l1)) 
  in
  let remove_doubles l = 
    List.fold_right 
      (fun x y -> if List.mem x y then y else x::y) l []
  in
    remove_doubles([lst]@snd(List.fold_left f (List.length lst, []) lst))