external rawData : (string * string) Js.Array.t = "rawData" [@@bs.val][@@bs.scope "window"]

open Language

module Spanish : LANGUAGE = 
struct
	let name = "Spanish"
	let native_name = "Castellano"

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
		| Root of string
    	| Conj of (tense * (person list) * (arerir list))
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
					| _ -> if (len < 2)
							then parse_state
						else begin
							match String.sub verb (len-2) 2 with
							| _ -> if (len < 3)
									then parse_state
								else begin
									match String.sub verb (len-3) 3 with
									| _ -> if (len < 4)
											then parse_state
										else begin
											match String.sub verb (len-4) 4 with
											| "ando" -> (("ando", Conj (Gerund, None, Ar::[]))::soFar, (String.sub (snd parse_state) 0 (len-4))))
											| _ -> if (len < 5)
													then parse_state
												else begin
													match String.sub verb (len-5) 5 with
													| "iendo" -> (("iendo", Conj (Gerund, None, Er::Ir::[]))::soFar, (String.sub (snd parse_state) 0 (len-5))))
													| _ -> if (len < 6)
															then parse_state
														else begin
															match String.sub verb (len-6) 6 with
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
				else (root, Root (root))::soFar

	let stem_verb (verb:string) : (string * morpheme) list = 
		let a = findReflexive verb in
		let b = findObject a in
		let c = findConjugation b in
				findRoot c

	(* MORPHEME DESCRIPTION HELPER FUNCTIONS *)
		let string_of_arerir (arerir: arerir) : string = 
		    match arerir with
		    | Ar -> "ar"
		    | Er -> "er"
		    | Ir -> "ir"

		let stringPerson (p: person) : string = 
		    match p with
		    | First -> "first-person singular"
		    | Second -> "second-person singular"
		    | Third -> "third-person singular"
		    | FirstPlural -> "first-person plural"
		    | SecondPlural -> "second-person plural"
		    | ThirdPlural -> "third-person plural"

		let rec stringPeople (lst: person list) : string =
		    match lst with
		    | hd::neck::[] -> (stringPerson hd) ^ " or " ^ (stringPerson neck)
		    | hd::tl -> (stringPerson hd) ^ " or " ^ (stringPeople tl)
		    | [] -> ""

		let stringGender (g: gender) : string =
		    match g with
		    | Masc -> "masculine"
		    | Fem -> "feminine"
		    | Neutral -> "genderless"

		let stringTense (t:tense) : string =
		    match t with
		    | Inf -> "infinitive"
		    | Present -> "present"
		    | Preterite -> "preterite"
		    | Imperfect -> "imperfect"
		    | Future -> "future"
		    | Conditional -> "conditional"
		    | Gerund -> "gerund"
		    | PluPerfect -> "pluscuam-perfect"
		    | PresPerfect -> "present-perfect"
		    | PretPerfect -> "preterite-perfect"
		    | FuturePerfect -> "future-perfect"
		    | PresSubj -> "subjunctive present"
		    | PretSubj -> "subjunctive preterite"
		    | ImperfectSubj -> "subjunctive imperfect"
		    | FutureSubj -> "subjunctive future"
		    | CondSubj -> "subjunctive conditional"
		    | PluPerfectSubj -> "subjunctive pluscuam-perfect"
		    | PresPerfSubj -> "subjunctive present-perfect" 
		    | PretPerfSubj -> "subjunctive preterite-perfect"
		    | FuturePerfSubj -> "subjunctive future-perfect"
		    | Aff -> "imperative"
		    | Neg -> "negative imperative"

		let tenseDesc (t:tense) : string = 
		(* used for the hover-definition of the tense *)
		    match t with
		    | Inf -> "basic form of a verb, without inflection binding to a subject or tense,\ne.g. \"to swim\""
		    | Present -> "form of a verb expressing an action that is current,\ne.g. \"I swim\""
		    | Preterite -> " preterite"
		    | Imperfect -> " imperfect"
		    | Future -> " future"
		    | Conditional -> " conditional"
		    | Gerund -> "form of a verb that functions as noun,\ne.g. \"Swimming is fun\""
		    | PluPerfect -> " pluscuam-perfect"
		    | PresPerfect -> " present-perfect"
		    | PretPerfect -> " preterite-perfect"
		    | FuturePerfect -> " future-perfect"
		    | PresSubj -> " subjunctive present"
		    | PretSubj -> " subjunctive preterite"
		    | ImperfectSubj -> " subjunctive imperfect"
		    | FutureSubj -> " subjunctive future"
		    | CondSubj -> " subjunctive conditional"
		    | PluPerfectSubj -> " subjunctive pluscuam-perfect"
		    | PresPerfSubj -> " subjunctive present-perfect" 
		    | PretPerfSubj -> " subjunctive preterite-perfect"
		    | FuturePerfSubj -> " subjunctive future-perfect"
		    | Aff -> " imperative"
		    | Neg -> " negative imperative"

		let lookupVerbDef (verb: string) : string = "GET RINGO TO ADD THE JAVASCRIPT"
		(* takes in the infinitive form of the verb and returns the English translation *)
		(* for regular verbs, the input to getVerbDef is just root^(string_of_arerir arerir)^reflexive *)

	let morpheme_def (morph:morpheme) : text_chunk list =
		let compose (root:string) (aeilst:arerir list) (reflex:reflexive): string list = 
			match aeilst with
			| hd::[] -> (root ^ string_of_arerir hd ^ (if reflex then "se" else ""))::[]
			| hd::tl::[] -> (root ^ string_of_arerir hd ^ (if reflex then "se" else ""))::(root ^ string_of_arerir tl ^ (if reflex then "se" else ""))::[]
			| hd::nk::tl::[] -> (root ^ string_of_arerir hd ^ (if reflex then "se" else ""))::(root ^ string_of_arerir nk ^ (if reflex then "se" else ""))::(root ^ string_of_arerir tl ^ (if reflex then "se" else ""))::[]
			| [] -> []
			| _ -> "input to Spanish.morpheme_def.compose not recognized"::[]
		in match morph with (* the root lookup in the dictionary currently only works on regular verbs *)
			| Root(root) -> lookupVerbDef (compose root (* need more here *) )
						(* need to try all members of the arerir list *)
			| Conj(tense, people, arerir) -> 
				(Plain (stringPeople people))::(WithDef ((stringTense tense), (tenseDesc tense)))::(Plain (" form of -" ^ (string_of_arerir arerir) ^ " verbs"))::[]
	    	| Object(person, gender) -> 
	    		(Plain (stringPerson person))::(Plain (stringGender gender))::(Plain "direct object pronoun")::[]
			| Reflexive -> (Plain "indicates the verb is reflexive")::[]

	let morpheme_color (morph:morpheme) : Color.t = 
		match morph with
		| Root(_) -> Color.get 0
		| Conj(_,_,_) -> Color.get 1
		| Object(_,_) -> Color.get 2
		| Reflexive -> Color.get 3

	let translate (parseList:(string * morpheme) list) : translation_result = LanguageNotSupported
end
