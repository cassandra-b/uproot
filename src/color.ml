type t = int

type scheme = (float * float * float) array

let get = Utils.id

exception NoSuchColor

let convert_color color scheme =
  if color >= Array.length scheme then
    raise NoSuchColor
  else
    scheme.(color)
