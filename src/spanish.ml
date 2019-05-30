open Language

module Spanish : LANGUAGE = 
struct
  type morpheme = Todo

  let stem_verb (verb:string) : (string * morpheme) list = Utils.todo ()

  let morpheme_def (morph:morpheme) : text_chunk list = Utils.todo ()

  let morpheme_color (morph:morpheme) : Color.t = Utils.todo ()

  let translate (parseList:(string * morpheme) list) : translation_result = Utils.todo ()
end
