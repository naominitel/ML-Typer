open Codemap
open Utils

type err = string Codemap.spanned

type 'a t =
    | Ok of 'a
    | Err of err LazyList.t

let bind f res = match res with
    | Ok r -> f r
    | Err r -> Err r

let map f res = match res with
    | Ok r -> Ok (f r)
    | Err r -> Err r

let map2 f res1 res2 = match (res1, res2) with
    | (Ok r1, Ok r2)          -> Ok (f r1 r2)
    | (Err e1, Err e2)        -> Err (LazyList.append e1 e2)
    | (Err e, _) | (_, Err e) -> Err e

let map3 f res1 res2 res3 =
    map2 ($) (map2 f res1 res2) res3

let sequence res = List.fold_left (map2 (fun acc r -> r :: acc)) (Ok []) res

let mapn f l =
    sequence (List.map (map f) l)

let new_err sp err = Err (LazyList.cons { sp = sp ; d = err } LazyList.nil)
