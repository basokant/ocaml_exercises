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
let equal (l1: 'a list) (l2: 'a list): bool =
  match List.compare compare l1 l2 with
  | 0 -> true
  | _ -> false

(** Find out whether a list is a palindrome *)
let is_palindrome (list: 'a list): bool = equal list (rev list)

type 'a node =
  | One of 'a 
  | Many of 'a node list

(** Flatten a nested list structure *)
let flatten (list: 'a node list): 'a list =
  let rec aux acc list =
    match list with
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  rev (aux [] list)

(** Eliminate consecutive duplicates of list elements *)
let rec compress (list: 'a list): 'a list =
  match list with
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller;;
