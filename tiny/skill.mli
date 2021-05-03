module G : sig
  type cute

  type agile

  type perceptive

  type strong

  type tough

  type smart

  type fast
end

module S : sig
  type g
  (** General skill *)

  type s
  (** Specialized skill *)
end

type 'g group =
  | Cute : G.cute group
  | Agile : G.agile group
  | Perceptive : G.perceptive group
  | Strong : G.strong group
  | Tough : G.tough group
  | Smart : G.smart group
  | Fast : G.fast group

type 's ty = Gen : S.g ty | Spec : S.s ty

type (_, _) t =
  | Melee_wpn : (G.agile, S.g) t
  | Ranged_wpn : (G.agile, S.g) t
  | Thrown_wpn : (G.agile, S.g) t
  | Unarmed_combat : (G.agile, S.g) t
  | Stealth : (G.agile, S.g) t
  | Balance : (G.agile, S.g) t
  | Riding : (G.agile, S.g) t
  | Dodging : (G.agile, S.g) t
  | Climbing : (G.agile, S.g) t
  | Handling : (G.agile, S.g) t
  | Aerial : (G.agile, S.g) t
  | Acrobatics : (G.agile, S.s) t
  | War_machine : (G.agile, S.s) t
  | KnockOut : (G.agile, S.s) t
  | Driving_land : (G.agile, S.s) t
  | Counterfeit : (G.agile, S.s) t
  | Explosives : (G.agile, S.s) t
  | Swimming : (G.agile, S.s) t
  | Parrying : (G.agile, S.s) t
  | Pocketpicking : (G.agile, S.s) t
  | Piloting_air : (G.agile, S.s) t
  | Piloting_sea : (G.agile, S.s) t
  | Repair : (G.agile, S.s) t
  | Locksmith : (G.agile, S.s) t
  | Aiming : (G.agile, S.s) t
  | Biology : (G.smart, S.g) t
  | Chemistry : (G.smart, S.g) t
  | Deduction : (G.smart, S.g) t
  | Electricity : (G.smart, S.g) t
  | Town : (G.smart, S.g) t
  | Etiquette : (G.smart, S.g) t
  | History : (G.smart, S.g) t
  | Mechanics : (G.smart, S.g) t
  | Memorizing : (G.smart, S.g) t
  | Religion : (G.smart, S.g) t
  | Remembering : (G.smart, S.g) t
  | Care_toy : (G.smart, S.g) t
  | Crafting : string -> (G.smart, S.s) t
  | Foreign_lang : string -> (G.smart, S.s) t
  | Law : (G.smart, S.s) t
  | Economics : (G.smart, S.s) t
  | Electronics : (G.smart, S.s) t
  | CS : (G.smart, S.s) t
  | Firefighting : (G.smart, S.s) t
  | Pharmacology : (G.smart, S.s) t
  | Care_human : (G.smart, S.s) t
  | Athleticism : (G.strong, S.g) t
  | Smash : (G.strong, S.g) t
  | Lift : (G.strong, S.s) t
  | Stamina : (G.tough, S.g) t
  | Threaten : (G.tough, S.g) t
  | Cold_blood : (G.tough, S.g) t
  | Willpower : (G.tough, S.g) t
  | Torture_res : (G.tough, S.s) t
  | Search : (G.perceptive, S.g) t
  | Listen : (G.perceptive, S.g) t
  | Nose : (G.perceptive, S.g) t
  | Barter : (G.perceptive, S.g) t
  | Navigation : (G.perceptive, S.g) t
  | Strategy : (G.perceptive, S.g) t
  | Tracking : (G.perceptive, S.g) t
  | Box_knowledge : (G.perceptive, S.s) t
  | Psychology : (G.perceptive, S.s) t
  | Tailing : (G.perceptive, S.s) t
  | Survival : (G.perceptive, S.s) t
  | Charm : (G.cute, S.g) t
  | Bluff : (G.cute, S.g) t
  | Relations : (G.cute, S.g) t
  | Seduction : (G.cute, S.g) t
  | Speech : (G.cute, S.s) t
  | Box_politics : (G.cute, S.s) t
  | Run : (G.fast, S.g) t
  | Reaction : (G.fast, S.g) t
  | Sprint : (G.fast, S.g) t
  | Act_first : (G.fast, S.s) t

val display : ('c, 's) t -> string

val options : 'c group -> 's ty -> ('c, 's) t list

val group_name : 'c group -> string
