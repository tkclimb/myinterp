type 'a t = (string * 'a) list

exception Not_found

let empty = []
let extend k v env = (k,v)::env

let lookup k env =
  try List.assoc k env with
    Not_found -> raise Not_found

let rec map f = function
  | [] -> []
  | (k,v)::rest -> (k,f v)::map f rest

let rec fold_right f env a =
  match env with
  | [] -> a
  | (_,v)::rest -> f v (fold_right f rest a)