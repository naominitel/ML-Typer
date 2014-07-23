(* Utility functions *)

include Batteries

let ($) f a = f a

let string_of_list lst ?sep:(s=" ; ") tostr = match lst with
    | []           -> ""
    | (car :: cdr) -> (List.fold_left
                          (fun acc e -> (Printf.sprintf "%s%s%s" acc s (tostr e)))
                          (tostr car) cdr)
