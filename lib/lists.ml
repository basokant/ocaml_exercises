(** Return the last element of a list *)
let rec last (list: 'a list) = 
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

(** Return the last 2 elements of a list *)
let rec last_two (list: 'a list) =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t;;

(** Return the nth element of a list *)
let rec nth k (list: 'a list) =
  match list with
  | [] -> None
  | h :: t -> if k = 0 then Some h else nth (k-1) t;;

(** Return the length of a list *)
let length (list: 'a list) =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0 list;;


