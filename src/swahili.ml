open Language

module Swahili : LANGUAGE =
struct
	let name = "Swahili"
	let native_name = "Kiswahili"

	type person = 
	    | First
	    | Second
	    | Third
	    | FirstPlural
	    | SecondPlural
	    | ThirdPlural

	type tense =
		| Past
		| Present
		| Future
		| PPerfect (* me- tense, present perfect on passive verbs and past perfect on active verbs *)
		| Habitual
		| NotYet (* ja- tense, always negative *)
		(* ADD MORE TENSES *)

	type negative = bool (* false by default, tru if a negative is present *)
	type active = bool (* false if it is a passive verb *)

	type morpheme =
		| Neg
		| Subject of person
		| Tense of tense (* there may be no tense, in which case it is an imperative command *)
		| Root of string * active

	let stem_verb (verb:string) : (string * morpheme) list = Utils.todo ()
	
	let morpheme_def (morph:morpheme) : text_chunk list = (Plain (""))::[]

	let morpheme_color (morph:morpheme) : Color.t = 
        (*
		match morph with
		|  -> Color.get 0
		|  -> Color.get 1
		|  -> Color.get 2
		|  -> Color.get 3
        *)
        Utils.todo ()

	let translate (parseList:(string * morpheme) list) : translation_result = LanguageNotSupported

end
