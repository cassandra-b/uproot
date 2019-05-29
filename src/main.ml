let (%) = Utils.(%)

let () =
  Printexc.record_backtrace true;
  (* Ui.set_func (Rendering.render_morphemes % Parser.stem) *)
  Ui.set_func (fun s -> Ui.Text(Ui.Rgb(0.0, 0.0, 0.0), 1.0, s))
