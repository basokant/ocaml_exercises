(** Return the last element of a list *)
let rec last list = 
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

let rec last_two list =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t;;
