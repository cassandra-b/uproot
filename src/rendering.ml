open Types
open Ui

let root_color = Colors.lilac
let ending_color = Colors.cyan
let object_color = Colors.rose
let reflexive_color = Colors.yellow
let border_color = Colors.magenta

let morpheme_size = 2.5
let border_width = 5.0
let definition_size = 2.0
let translation_size = 2.5

let color_of_meaning = function
  | Root(_, _, _) -> root_color
  | Ending(_, _, _) -> ending_color
  | Object(_, _) -> object_color
  | Reflexive(_) -> reflexive_color

let render_morpheme str morpheme =
  Text(color_of_meaning meaning, morpheme_size, str)

let render_definition str morpheme =
  let color = color_of_meaning morpheme in
  let component_of_text_chunk = function
    | Plain(s) -> Text(color, definition_size, s)
    | WithDefinition(s, help) -> HelpText(color, definition_size, s, help)
  in
  Box(Horz,
    [ Text(color, definition_size, "\"" ^ str ^ "\" -")
    , Space(definition_size)
    ] @ List.map component_of_text_chunk (Language.getMorphemeDef morpheme))

let render_morphemes (morphemes: (string * morpheme) list) : component =
  let bordered c = BorderBox(border_color, border_width, c) in
  let morphemes_component = bordered (Box(Horz, List.map2 render_morpheme morphemes)) in
  let definition_components = List.map2 render_definition morphemes in
  let translation_components = Utils.intersperse (Space(translation_size))
    [ Text(root_color, translation_size, "Translation")
    ; Text(ending_color, translation_size, "Not")
    ; Text(object_color, translation_size, "Implemented")
    ] in
  let translation_component = bordered (Box(Horz, translation_components)) in
  Box(Vert, morphemes_component::definition_components@[translation_component])
