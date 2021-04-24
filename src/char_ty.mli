module Kind : sig
  type pet

  type toy

  type _ t = Pet : pet t | Toy : toy t
end

module Power : sig
  type _ t =
    | Dream : _ t
    | Strength : _ t
    | Pretty : _ t
    | Flight : _ t
    | Intimidation : _ t
    | Claws : Kind.pet t
    | Leap : Kind.pet t
    | HumanBFF : Kind.pet t
    | Voice_module : Kind.toy t
    | Accessories : Kind.toy t
    | Imaginary_friend : Kind.toy t
    | Authority : Kind.toy t
    | Elite_skill : Kind.toy t
    | Construction : Kind.toy t
    | Growth : Kind.toy t
    | Teddy : Kind.toy t
    | Odd_shape : Kind.toy t
    | Gigantism : Kind.toy t
    | Healing : Kind.toy t
    | Smarts : Kind.toy t
    | Licensed : Kind.toy t
    | Motorized : Kind.toy t
    | Multiplicity : Kind.toy t
    | Punching_ball : Kind.toy t
    | Slinky : Kind.toy t
    | Tough : Kind.toy t
    | Transformation : Kind.toy t
    | Malleable : Kind.toy t

  val display : 'k t -> string

  val options : 'k Kind.t -> 'k t list

  val describe :
    'k t -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
end

module Skill : sig
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
    | Mecanics : (G.smart, S.g) t
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

  val describe :
       ('c, 's) t
    -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
end

module Flaw : sig
  module Phoby : sig
    type t =
      | Darkness
      | Attic
      | Basement
      | Isolation
      | Claustrophobic
      | That_one_kid
      | Adults
      | Scary_places
      | Insects
      | Water
      | Outside
      | Thunder
  end

  type _ t =
    | Shorty : Kind.toy t
    | First_age : Kind.toy t
    | Minuscule : Kind.toy t
    | Mute : Kind.toy t
    | Handless : Kind.toy t
    | Phobic : Phoby.t -> Kind.toy t
    | Batteries : Kind.toy t
    | Prima_donna : Kind.toy t
    | Factory_setting : Kind.toy t
    | Traumatized : Kind.toy t
    | Unlucky : Kind.pet t
    | Glutton : Kind.pet t
    | Instincts : Kind.pet t
    | Harmless : Kind.pet t
    | Nemesis : Kind.pet t
    | Sadistic : Kind.pet t
    | Snoozer : Kind.pet t
    | Forgetful : Kind.pet t

  val display : 'k t -> string

  val roll : 'k Kind.t -> 'k t

  val cp : _ t -> int

  val options : 'k Kind.t -> 'k t list

  val codec : 'k Kind.t -> (string -> 'k t) * ('k t -> string)

  val describe :
    'k t -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
end

type 'g skill_group =
  { skill_group_bonus : int
  ; skill_group : 'g Skill.group
  ; skill_gen_skills : ('g, Skill.S.g) Skill.t list
  ; skill_spec_skills : ('g, Skill.S.s) Skill.t list
  }

type char_skills =
  { smart_skills : Skill.G.smart skill_group
  ; agile_skills : Skill.G.agile skill_group
  ; strong_skills : Skill.G.strong skill_group
  ; tough_skills : Skill.G.tough skill_group
  ; perceptive_skills : Skill.G.perceptive skill_group
  ; cute_skills : Skill.G.cute skill_group
  ; fast_skills : Skill.G.fast skill_group
  }

type char_kind = Pet of Kind.pet Kind.t | Toy of Kind.toy Kind.t

val pet : char_kind

val toy : char_kind

type 'k char_build =
  { name : string
  ; powers : 'k Power.t list
  ; flaws : 'k Flaw.t list
  ; skills : char_skills
  }

val new_pet : unit -> Kind.pet char_build

val new_toy : unit -> Kind.toy char_build
