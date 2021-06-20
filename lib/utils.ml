let rev_list l =
  let rec _rev_list acc = function
    | [] -> acc
    | x::xs -> _rev_list (x::acc) xs in
  _rev_list [] l