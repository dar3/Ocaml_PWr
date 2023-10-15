
(* Zad.1 *)
let rec flatten(lt) = (
  if lt = [] then [] 
    else List.hd lt @ flatten(List.tl lt)
);;

(* Zad. 2 *)

    let rec count(a, lt) = (
      let abc = (fun (y, z) -> if y = z then 1 else 0) in
      if lt = [] then 0
       else abc(a, List.hd lt) + count(a, List.tl lt)
  );;

  (* Zad. 3 *)

  let rec replicate(a, num) = (
    if num <= 0 then []
    else [a] @ replicate(a, num-1)
);;


(* Zad. 4 *)

let rec sqrList (item) = (
    if item = [] then [] else 
      [List.hd item * List.hd item] @ sqrList(List.tl item)

);;

(* Zad. 5 *)

let palindrome list = (
  list = List.rev list
);;

(* Zad. 6 *)

let rec listLength (lst) = (
  if lst = [] then 0 else 
    1 + listLength(List.tl lst)
);;

(* Zad. 7 *)
(* nw *)

let rec T (n) = (
  let c = 1
  if n = 1 then 1
  else c * int_of_float (log (float_of_int n) /. log 2.0) + T (n / 2)
);;

 
