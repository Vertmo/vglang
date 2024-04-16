(* Utilities *)

(** Identifiers of the language *)
type ident = string
[@@deriving show]

type filename = string
[@@deriving show]

module IdMap = Map.Make(String)

let idmap_to_list map = List.of_seq (IdMap.to_seq map)

let find_with_msg msg map id =
  try IdMap.find id map
  with Not_found -> failwith (Printf.sprintf "Unknown %s %s" msg id)

(* Of course my state is monadic, why ? *)

(** states can be heterogenous :) *)
type ('a, 'st1, 'st2) hetmon = 'st1 -> ('a * 'st2)

(** but usually homegenous *)
type ('a, 'st) mon = ('a, 'st, 'st) hetmon

(** ret, bind, etc *)
let ret x : ('a, 'st) mon = fun st -> (x, st)
let ( let* ) (o: ('a, 'st) mon) (f : 'a -> ('b, 'st) mon) =
  fun st -> let (l, st) = o st in f l st
let omap (f : 'a -> ('b, 'st) mon) (l : 'a list) : ('b list, 'st) mon =
  fun m -> List.fold_right (fun a (l, m) -> (let* b = f a in ret (b::l)) m) l ([], m)
let run (f : ('a, 'st) mon) = fun st -> fst (f st)
