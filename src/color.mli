(** The type of a color. *)
type t

(** A color scheme. *)
type scheme

(** Gets the color with the given number. It is guaranteed that colors 0
 * through 9 (inclusive) are defined for each color scheme. If an undefined
 * color is used, an exception will be thrown. *)
val get : int -> t

(** The exception thrown when an undefined color is referred to. *)
exception NoSuchColor

(** Converts a color to red, green, and blue values, between 0 and 1. Throws an
 * exception if an undefined color is used. *)
val convert_color : t -> scheme -> float * float * float
