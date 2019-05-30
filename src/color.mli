(** The type of a color. *)
type t

(** A color scheme. *)
type scheme

(** Gets the color with the given number. It is guaranteed that colors 0
 * through 9 (inclusive) are defined for each color scheme. If an undefined
 * color is used, an exception will be thrown. *)
val get : int -> t

(** Creates a color from a hex string. *)
val create : string -> t

(** The exception thrown when an undefined color is referred to. *)
exception NoSuchColor

(** Converts a color to an RGB hex string, e.g. "#7f00ff". *)
val convert : scheme -> t -> string

(** The default color scheme. *)
val default_scheme : scheme
