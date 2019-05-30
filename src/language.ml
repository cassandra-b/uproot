(** A chunk of text to be shown in a line in the UI.
 *
 * To allow adding definitions to some parts of a sentence, `text_chunk list`s
 * are used. For example, one would represent the sentence "Foo Bar Baz" with
 * "Bar" having a definition of "A man walks into a bar and says 'Ouch!'" as
 *
 * ```
 * [ Plain("Foo")
 * ; WithDef("Bar", "A man walks into a bar and says 'Ouch!'")
 * ; Plain("Baz")
 * ]
 * ```
 *
 * Note that the spaces are not present; due to the way HTML treats spaces,
 * spaces are explicitly inserted by the renderer between every two elements of
 * the `text_chunk list`. This means that `[Plain("Fo"); Plain("o")]` will
 * render as `Fo o` rather than `Foo`.
 * *)
type text_chunk
  = Plain of string
  | WithDef of string * string

(** The result of translating. Duh. *)
type translation_result
  = Ok of (Color.t * string) list
  | LanguageNotSupported
  | WordNotFound

module type LANGUAGE = sig
  type morpheme

  (** Breaks a verb up into morphemes.
   *
   * For example, in Spanish, the verb "corro" might be stemmed as
   *
   * ```
   * [ ("corr", ...)
   * ; ("o", ...)
   * ]
   * ```
   * *)
  val stem_verb : string -> (string * morpheme) list

  (** Gets the definition of a morpheme to be shown in the UI.  *)
  val morpheme_def : morpheme -> text_chunk list

  (** Gets the color a morpheme should be displayed in. *)
  val color_of_morpheme : morpheme -> Color.t

  (** Provides a rough translation of a verb. *)
  val translate : (string * morpheme) list -> translation_result
end
