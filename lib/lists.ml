(** Return the last element of a list *)
let rec last list = 
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;
