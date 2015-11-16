def map =
    fun f -> fun l ->
        if l = [] then []
        else (f (hd l)) :: (map f (tl l))

def a =
    let lst = [1 ; 2 ; 3] in
    let a = map (fun x -> print_int x) lst in

    let lst = map (fun x -> x + 1 ) lst in
    let a = map (fun x -> print_int x) lst in

    ()
