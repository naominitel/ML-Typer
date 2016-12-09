def map =
    fun f -> fun l ->
        if l = [] then []
        else (f (fst l)) :: (map f (snd l))

def _ =
    let lst = [1 ; 2 ; 3] in
    let _ = map (fun x -> print_int x) lst in

    let lst = map (fun x -> x + 1 ) lst in
    let _ = map (fun x -> print_int x) lst in

    ()
