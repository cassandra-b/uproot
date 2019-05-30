type t = int

type scheme = (float * float * float) array

let get = Utils.id

exception NoSuchColor

let convert_color color scheme =
  if color > Arrays.length scheme then
    scheme.(color)
  else
    raise NoSuchColor
