(** The type of a theme. *)
type t =
  { border_color : Color.t
  ; border_width : float
  ; definition_size : float
  ; error_color : Color.t
  ; morpheme_colors : Color.scheme
  ; morpheme_size : float
  ; translation_size : float
  }

(** The default theme. *)
val default : t
