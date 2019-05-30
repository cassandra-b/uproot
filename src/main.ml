open Language
open Renderer 

let () =
  Printexc.record_backtrace true;
  let module R = Renderer(Pirate) in
  Ui.set_func R.render_verb
