external log : 'a -> unit = "log" [@@bs.val][@@bs.scope "console"]

let (%) g f x = g (f x)

let rec first_some f = function
  | [] -> None
  | h::t -> (match f h with
            | Some(x) -> Some(x)
            | None -> first_some f t)

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
