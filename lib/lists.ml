(** Return the last element of a list *)
let rec last (list: 'a list): 'a option = 
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

(** Return the last 2 elements of a list *)
let rec last_two (list: 'a list): ('a * 'a) option =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t;;

(** Return the nth element of a list *)
let rec nth (list: 'a list) (k: int): 'a option =
  match list with
  | [] -> None
  | h :: t -> if k = 0 then Some h else nth t (k-1);;

(** Return the length of a list *)
let length (list: 'a list): int =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0 list;;

(** Reverse a list *)
let rec rev (list: 'a list): 'a list =
  match list with
  | [] -> []
  | h :: t -> (rev t) @ [h];;

(** Find out if two lists are equal *)
let rec equal (l1: 'a list) (l2: 'a list): bool =
  match (l1, l2) with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 -> h1 = h2 && equal t1 t2
  | _, _ -> false

(** Find out whether a list is a palindrome *)
let is_palindrome (list: 'a list): bool = equal list (rev list)
