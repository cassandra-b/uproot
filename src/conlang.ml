open Language

type tense = 
		| Gerund (* functions as noun *)
		| Participle (* functions as adjective *)
		| Imperative (* command *)
	| Present
	| Recent
	| LongAgo
	| Soon
	| SomeDay
	| Habitual
	| Already (* "not yet" when negated *)

type subject =
	| Inf
	| First
	| Second
	| Third
	| IncWe
	| ExcWe

type evidence = 
	| Fact
	| Seen
	| Clues
	| Hearsay
	| Assumed
	| Belief
	| Wish

type plural =
	| One
	| Some
	| Many

type verb = (string * string * string)

let tenseEnglish tns = 
	match tns with
	| Gerund -> "gerund"
	| Participle -> "participle"
	| Present -> "present"
	| Recent -> "recent"
	| LongAgo -> "long ago"
	| Soon -> "soon"
	| SomeDay -> "some day"
	| Habitual -> "habitual"
	| Imperative -> "imperative"
	| Already -> "already"

let tenseSuffix tns = 
	match tns with
	| Gerund -> "ang"
	| Participle -> "aj"
	| Present -> ""
	| Recent -> "re"
	| LongAgo -> "dere"
	| Soon -> "fa"
	| SomeDay -> "defa"
	| Habitual -> "hubu"
	| Imperative -> "da"
	| Already -> "ya"

let evEnglish ev =
	match ev with
	| Fact -> "it is fact"
	| Seen -> "the speaker was present and saw it happen"
	| Clues -> "it can be deduced from obvious clues"
	| Hearsay -> "someone else told it to the speaker"
	| Assumed -> "it is a mere assumption"
	| Belief -> "it is the speaker's personal belief"
	| Wish -> "the speaker wishes it were true"

let evSuffix ev =
	match ev with
	| Fact -> "la"
	| Seen -> "to"
	| Clues -> "de"
	| Hearsay -> "sha"
	| Assumed -> "ma"
	| Belief -> "pa"
	| Wish -> "kwa"

let pluralEnglish pl =
	match pl with
	| One -> "one"
	| Some -> "some"
	| Many -> "many"

let pluralPrefix pl =
	match pl with
	| One -> ""
	| Some -> "ma"
	| Many -> "wa"

let subjectEnglish sbj =
	match sbj with
	| Inf -> "infinitive"
	| First -> "first-person singular"
	| Second -> "second-person singular"
	| Third -> "third-person singular"
	| IncWe -> "inclusive we (first-person plural)"
	| ExcWe -> "exclusive we (first-person plural)"

let subjectAffix sbj =
	match sbj with
	| Inf -> "e"
	| First -> "i"
	| Second -> "u"
	| Third -> "a"
	| IncWe -> "iu"
	| ExcWe -> "ia"

let fst3 (xyz:verb) : string = match xyz with | (x,_,_) -> x
let snd3 (xyz:verb) : string = match xyz with | (_,y,_) -> y
let thd3 (xyz:verb) : string = match xyz with | (_,_,z) -> z

let conjugate (vb:verb) (tns:tense) (sbj:subject) (ev:evidence) (pl:plural): string =
	let infix = subjectAffix sbj 
	in  (pluralPrefix pl) ^ 
		(fst3 vb) ^ infix ^ 
		(snd3 vb) ^ infix ^ 
		(thd3 vb) ^ (tenseSuffix tns) ^ (evSuffix ev)
(* 
module Conlang : LANGUAGE = 
struct
	type morpheme = 
		| 
		|
		|

	let name = "Conlang"
	let native_name = "Kumerkalom"

	let stem_verb vrb : (string * morpheme) list =

	let morpheme_def morph : text_chunk list =

	let morpheme_color morphe : Color.t =

	let translate (morphemes: (string * morpheme) list) : translation_result =


end *)