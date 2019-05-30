type color
  = LitColor of string
  | Rgb of float * float * float

type layout = Horz | Vert

type size = float

type width = float

type help = string

type component
  = BorderBox of color * width * component
  | Box of layout * component list
  | HelpText of color * size * string * help
  | Space of float
  | Text of color * size * string

val set_func : (string -> component) -> unit
