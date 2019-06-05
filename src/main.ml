open Language
open Renderer 
open Spanish

let () =
  Printexc.record_backtrace true;
  let module R = Renderer(Spanish) in
  Ui.set_func R.render_verb
