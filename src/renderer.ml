open Language
open Theme
open Ui
open Utils

module Renderer(Language: LANGUAGE) : sig
  val render_morphemes : Theme.t -> (string * Language.morpheme) list -> component
  val render_verb : Theme.t -> string -> component
end = struct
  type morpheme = Language.morpheme

  let morpheme_color (theme: Theme.t) : morpheme -> string =
    Color.convert theme.morpheme_colors % Language.morpheme_color

  let render_morpheme (theme: Theme.t) ((str, morpheme): string * morpheme) : component =
    Text(morpheme_color theme morpheme, theme.morpheme_size, str)

  let render_definition (theme: Theme.t) ((str, morpheme): string * morpheme) : component =
    let color = morpheme_color theme morpheme in
    let component_of_text_chunk = function
      | Plain(s) -> Text(color, theme.definition_size, s)
      | WithDef(s, help) -> HelpText(color, theme.definition_size, s, help)
    in
    let parts =
      [ Text(color, theme.definition_size, "\"" ^ str ^ "\" -")
      ; Space(theme.definition_size)
      ] @ List.map component_of_text_chunk (Language.morpheme_def morpheme)
    in
    Box(Horz, false, intersperse (Space(theme.definition_size)) parts)

  let render_morphemes (theme: Theme.t) : (string * morpheme) list -> component =
    let convert_color = Color.convert theme.morpheme_colors in function
    | [] -> Text(convert_color theme.error_color, theme.morpheme_size, "Stemming error")
    | morphemes ->
      let border_color = convert_color theme.border_color in
      let bordered c = BorderBox(border_color, theme.border_width, c) in

      let morphemes_components = List.map (render_morpheme theme) morphemes in
      let morphemes_component = bordered (Box(Horz, true, morphemes_components)) in

      let definition_components = List.map (render_definition theme) morphemes in

      let translation_component = (match Language.translate morphemes with
        | Ok(parts) -> parts
        | LanguageNotSupported -> [(theme.error_color, "Translation not supported in this language")]
        | WordNotFound -> [(theme.error_color, "Word not found")])
        |> List.map (fun (c, s) -> Text(convert_color c, theme.translation_size, s))
        |> intersperse (Space(theme.translation_size))
        |> (fun cs -> Box(Horz, true, cs))
        |> bordered
      in

      Box(Vert, true, morphemes_component::definition_components@[translation_component])

  let render_verb (theme: Theme.t) : string -> component =
    render_morphemes theme % Language.stem_verb
end
