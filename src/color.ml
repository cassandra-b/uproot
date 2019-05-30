type t
  = Pallete of int
  | Rgb of string

type scheme = string array

let get i = Pallete(i)

let create s = Rgb(s)

exception NoSuchColor

let convert scheme = function
  | Pallete(i) -> 
    if i >= Array.length scheme then
      raise NoSuchColor
    else
      scheme.(i)
  | Rgb(s) -> s

(* TODO: Define thru 9 colors. *)
let default_scheme = [|"#D14EFF"; "#76EFF0"; "#FF94E8"; "#FBFB6B"; "#D00EB8"|]
