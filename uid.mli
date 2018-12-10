type t = string

val show : t -> string
val eq : t -> t -> bool
val intern : string -> t
val gensym : unit -> t
