
type t

val make : int -> t

val funct : t -> (int -> 'a) -> int -> 'a
