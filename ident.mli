type t

val show : t -> string
val eq : t -> t -> bool
val intern : string -> t
val gensym : unit -> t
val make : string -> t
