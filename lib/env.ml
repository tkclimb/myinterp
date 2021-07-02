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


let rec print_env_rec = function
  | [] -> print_string "\n";
  | (k,_)::rest -> Printf.printf "%s, " k;
    print_env_rec rest

let print_env env =
  print_string "{ ";
  print_env_rec env;
  print_string "}"
