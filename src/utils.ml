external log : 'a -> unit = "log" [@@bs.val][@@bs.scope "console"]

let (%) g f x = g (f x)

let id x = x

let rec intersperse x = function
  | [] -> []
  | [y] -> [y]
  | h::t -> h::x::intersperse x t

let log' x = log x; x

let option_map f = function
  | Some(x) -> Some(f x)
  | None -> None

let todo () =
  Js.Exn.raiseError "TODO"
