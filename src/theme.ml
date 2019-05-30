type t =
  { border_color : Color.t
  ; border_width : float
  ; definition_size : float
  ; error_color : Color.t
  ; morpheme_colors : Color.scheme
  ; morpheme_size : float
  ; translation_size : float
  }

let default =
  { border_color = Color.create "#A02A2A"
  ; border_width = 5.0
  ; definition_size = 2.0
  ; error_color = Color.create "#FF0000"
  ; morpheme_colors = Color.default_scheme
  ; morpheme_size = 2.5
  ; translation_size = 2.5
  }
