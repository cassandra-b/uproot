val (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val first_some : ('a -> 'b option) -> 'a list -> 'b option
val id : 'a -> 'a
val intersperse : 'a -> 'a list -> 'a list
val log : 'a -> unit
val log' : 'a -> 'a
val option_map : ('a -> 'b) -> 'a option -> 'b option
val todo : unit -> 'a
