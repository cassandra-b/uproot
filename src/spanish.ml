open Language

module Spanish : LANGUAGE = 
struct
	let name = "Spanish"

	type tense = 
    (* indicatives*)
        | Inf 
        | Present 
        | Preterite 
        | Imperfect 
        | Future 
        | Conditional 
        | Gerund 
        (* perfect *)
        | PluPerfect 
        | PresPerfect 
        | PretPerfect 
        | FuturePerfect 
    (* subjunctives *)
        | PresSubj 
        | PretSubj 
        | ImperfectSubj 
        | FutureSubj 
        | CondSubj 
        (* perfect *)
        | PluPerfectSubj 
        | PresPerfSubj 
        | PretPerfSubj 
        | FuturePerfSubj
    (* imperatives *)
        | Aff
        | Neg

	type person = 
	    | First
	    | Second
	    | Third
	    | FirstPlural
	    | SecondPlural
	    | ThirdPlural

	type gender = 
	    | Masc
	    | Fem
	    | Neutral

	type arerir =
	    | Ar
	    | Er 
	    | Ir

	type regular = bool
	type reflexive = bool

	type morpheme = 
		| Root of (regular * arerir * reflexive)
    	| Conj of (tense * (person list) * arerir)
    	| Object of (person * gender)
    	| Reflexive

    type parsing = ((string * morpheme) list) * string

    (* STEM_VERB HELPER FUNCTIONS *)

    	let findReflexive (verb:string) : parsing * bool = 
			let len = String.length verb in
			let last2 = String.sub verb (len-2) 2 in
				match last2 with
				| "se" -> ((("se", Reflexive)::[], (String.sub verb 0 (len-2))), true)
				| _ -> (([], verb), false)

	let stem_verb (verb:string) : (string * morpheme) list = Utils.todo ()

	let morpheme_def (morph:morpheme) : text_chunk list = Utils.todo ()

	let morpheme_color (morph:morpheme) : Color.t = 
		match morph with
		| Root(_,_,_) -> Color.get 0
		| Conj(_,_,_) -> Color.get 1
		| Object(_,_) -> Color.get 2
		| Reflexive(_) -> Color.get 3

	let translate (parseList:(string * morpheme) list) : translation_result =
		LanguageNotSupported
end
