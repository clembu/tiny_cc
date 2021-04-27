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
