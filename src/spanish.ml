external rawData : (string * string) Js.Array.t = "rawData" [@@bs.val][@@bs.scope "window"]

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
		| Root
    	| Conj of (tense * (person list) * arerir)
    	| Object of (person * gender)
    	| Reflexive

    type parsing = ((string * morpheme) list) * string

    (* STEM_VERB HELPER FUNCTIONS *)

    	let findReflexive (verb:string) : parsing = 
    	(* takes in a verb and checks last two letters for a "se" *)
		(* returns the list of morphemes identified so far and the string left to be parsed *)
			let len = String.length verb in
				if (len < 3)
					then ([], verb)
				else match String.sub verb (len-2) 2 with
					| "se" -> (("se", Reflexive)::[], (String.sub verb 0 (len-2)))
					| _ -> ([], verb)

		let findObject (parse_state:parsing) : parsing =
		(* takes in a verb and checks last two/three letters for an object *)
		(* returns the list of morphemes identified so far and the string left to be parsed *)
			let len = String.length (snd parse_state) in
				if (len < 3) 
					then parse_state
				else match String.sub (snd parse_state) (len-2) 2 with
					| "lo" -> ((("lo", Object (Third, Male))::(fst parse_state), (String.sub (snd parse_state) 0 (len-2))))
					| "la" -> ((("la", Object (Third, Female))::(fst parse_state), (String.sub (snd parse_state) 0 (len-2))))
					| "le" -> ((("le", Object (Third, Neutral))::(fst parse_state), (String.sub (snd parse_state) 0 (len-2))))
					| "me" -> ((("me", Object (First, Neutral))::(fst parse_state), (String.sub (snd parse_state) 0 (len-2))))
					| "te" -> ((("te", Object (Second, Neutral))::(fst parse_state), (String.sub (snd parse_state) 0 (len-2))))
					| "os" -> ((("os", Object (SecondPlural, Neutral))::(fst parse_state), (String.sub (snd parse_state) 0 (len-2))))
					| _ -> begin
							match String.sub (snd parse_state) (len-3) 3 with
							| "los" -> ((("los", Object (ThirdPlural, Male))::(fst parse_state), (String.sub (snd parse_state) 0 (len-3))))
							| "las" -> ((("las", Object (ThirdPlural, Female))::(fst parse_state), (String.sub (snd parse_state) 0 (len-3))))
							| "les" -> ((("les", Object (ThirdPlural, Neutral))::(fst parse_state), (String.sub (snd parse_state) 0 (len-3))))
							| "nos" -> ((("nos", Object (FirstPlural, Neutral))::(fst parse_state), (String.sub (snd parse_state) 0 (len-3))))
							| _ -> parse_state
						end

		let findConjugation (parse_state:parsing) : parsing =
		(* takes in a verb and checks last few letters for specific conjugations *)
		(* returns the list of morphemes identified so far and the string left to be parsed *)
			let verb = snd parse_state in
			let soFar = fst parse_state in
			let len = String.length verb in
				if (len < 1) 
					then parse_state
				else match verb with
					|
					|
					| _ -> if (len < 2)
							then parse_state
						else begin
							match String.sub verb (len-2) 2 with
							|
							|
							| _ -> if (len < 3)
									then parse_state
								else begin
									match String.sub verb (len-3) 3 with
									|
									|
									| _ -> if (len < 4)
											then parse_state
										else begin
											match String.sub verb (len-4) 4 with
											| "ando" -> (("ando", Conj (Gerund, None, Ar))::soFar, (String.sub (snd parse_state) 0 (len-4))))
											| 
											| _ -> if (len < 5)
													then parse_state
												else begin
													match String.sub verb (len-5) 5 with
													|
													|
													| _ -> if (len < 6)
															then parse_state
														else begin
															match String.sub verb (len-6) 6 with
															|
															|
															| _ -> parse_state
															end
													end
											end
									end
							end

		let findRoot (parse_state:parsing) : (string * morpheme) list =
			let root = snd parse_state in
			let soFar = fst parse_state in
				if (String.length root < 1) 
					then soFar
				else (root, Root)::soFar
				

	let stem_verb (verb:string) : (string * morpheme) list = 
		let a = findReflexive verb in
		let b = findObject a in
		let c = findConjugation b in
				findRoot c

	(* MORPHEME DESCRIPTION HELPER FUNCTIONS *)

		let getVerbDef (verb: string) : string = "GET RINGO TO ADD THE JAVASCRIPT"
		(* takes in the infinitive form of the verb and returns the English translation *)
		(* for regular verbs, the input to getVerbDef is just root^(string_of_arerir arerir)^reflexive *)


	let morpheme_def (morph:morpheme) : text_chunk list = Utils.todo ()

	let morpheme_color (morph:morpheme) : Color.t = 
		match morph with
		| Root -> Color.get 0
		| Conj(_,_,_) -> Color.get 1
		| Object(_,_) -> Color.get 2
		| Reflexive -> Color.get 3

	let translate (parseList:(string * morpheme) list) : translation_result = LanguageNotSupported
end
