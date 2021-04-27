open Core

module Phoby = struct
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

  (** To append to "Scared of" *)
  let display = function
    | Darkness -> "the dark"
    | Attic -> "the attic"
    | Basement -> "the basement"
    | Isolation -> "being alone"
    | Claustrophobic -> "tight spaces"
    | That_one_kid -> "this one friend of The Child"
    | Adults -> "adults"
    | Scary_places -> "scary places"
    | Insects -> "insects"
    | Water -> "water"
    | Outside -> "the outside"
    | Thunder -> "thunder"

  let roll () =
    match Dice.d12 () with
    | 1 -> Darkness
    | 2 -> Attic
    | 3 -> Basement
    | 4 -> Isolation
    | 5 -> Claustrophobic
    | 6 -> That_one_kid
    | 7 -> Adults
    | 8 -> Scary_places
    | 9 -> Insects
    | 10 -> Water
    | 11 -> Outside
    | 12 -> Thunder
    | _ -> assert false
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

let display : type k. k t -> string = function
  | Shorty -> "Shorty"
  | First_age -> "First age toy"
  | Minuscule -> "Minuscule"
  | Mute -> "Mute"
  | Handless -> "Look Ma'! No hands!"
  | Phobic p -> "Scared of " ^ Phoby.display p
  | Batteries -> "Batteries not included"
  | Prima_donna -> "Prima Donna"
  | Factory_setting -> "Factory settings"
  | Traumatized -> "Traumatized"
  | Glutton -> "Snazz Jazz"
  | Instincts -> "Basic Instincts"
  | Harmless -> "Absolutely harmless"
  | Nemesis -> "Nemesis"
  | Unlucky -> "Jinx"
  | Sadistic -> "Big Meanie"
  | Snoozer -> "Snoozer"
  | Forgetful -> "Forgetful"

let roll : type k. k Kind.t -> k t = function
  | Kind.Pet -> (
      match Dice.d8 () with
      | 1 -> Glutton
      | 2 -> Instincts
      | 3 -> Harmless
      | 4 -> Nemesis
      | 5 -> Unlucky
      | 6 -> Sadistic
      | 7 -> Snoozer
      | 8 -> Forgetful
      | _ -> assert false )
  | Kind.Toy -> (
      match Dice.d10 () with
      | 1 -> Shorty
      | 2 -> First_age
      | 3 -> Minuscule
      | 4 -> Mute
      | 5 -> Handless
      | 6 -> Phobic (Phoby.roll ())
      | 7 -> Batteries
      | 8 -> Prima_donna
      | 9 -> Factory_setting
      | 10 -> Traumatized
      | _ -> assert false )

let cp : type k. k t -> int = function
  | Shorty -> 2
  | First_age -> 1
  | Minuscule -> 2
  | Mute -> 2
  | Handless -> 2
  | Phobic _ -> 1
  | Batteries -> 2
  | Prima_donna -> 1
  | Factory_setting -> 1
  | Traumatized -> 1
  | Glutton -> 2
  | Instincts -> 1
  | Harmless -> 2
  | Nemesis -> 2
  | Unlucky -> 2
  | Sadistic -> 1
  | Snoozer -> 2
  | Forgetful -> 1

let options : type k. k Kind.t -> k t list = function
  | Kind.Pet ->
      [ Glutton
      ; Instincts
      ; Harmless
      ; Nemesis
      ; Unlucky
      ; Sadistic
      ; Snoozer
      ; Forgetful
      ]
  | Kind.Toy ->
      [ Shorty
      ; First_age
      ; Minuscule
      ; Mute
      ; Handless
      ; Phobic Phoby.Darkness
      ; Phobic Phoby.Attic
      ; Phobic Phoby.Basement
      ; Phobic Phoby.Isolation
      ; Phobic Phoby.Claustrophobic
      ; Phobic Phoby.That_one_kid
      ; Phobic Phoby.Adults
      ; Phobic Phoby.Scary_places
      ; Phobic Phoby.Insects
      ; Phobic Phoby.Water
      ; Phobic Phoby.Outside
      ; Phobic Phoby.Thunder
      ; Batteries
      ; Prima_donna
      ; Factory_setting
      ; Traumatized
      ]

let codec : type k. k Kind.t -> (string -> k t) * (k t -> string) = function
  | Kind.Pet -> (
      ( (function
        | "1" -> Glutton
        | "2" -> Instincts
        | "3" -> Harmless
        | "4" -> Nemesis
        | "5" -> Unlucky
        | "6" -> Sadistic
        | "7" -> Snoozer
        | "8" -> Forgetful
        | _ -> assert false )
      , function
        | Glutton -> "1"
        | Instincts -> "2"
        | Harmless -> "3"
        | Nemesis -> "4"
        | Unlucky -> "5"
        | Sadistic -> "6"
        | Snoozer -> "7"
        | Forgetful -> "8"
        | _ -> assert false ) )
  | Kind.Toy -> (
      ( (function
        | "1" -> Shorty
        | "2" -> First_age
        | "3" -> Minuscule
        | "4" -> Mute
        | "5" -> Handless
        | "61" -> Phobic Phoby.Darkness
        | "62" -> Phobic Phoby.Attic
        | "63" -> Phobic Phoby.Basement
        | "64" -> Phobic Phoby.Isolation
        | "65" -> Phobic Phoby.Claustrophobic
        | "66" -> Phobic Phoby.That_one_kid
        | "67" -> Phobic Phoby.Adults
        | "68" -> Phobic Phoby.Scary_places
        | "69" -> Phobic Phoby.Insects
        | "610" -> Phobic Phoby.Water
        | "611" -> Phobic Phoby.Outside
        | "612" -> Phobic Phoby.Thunder
        | "7" -> Batteries
        | "8" -> Prima_donna
        | "9" -> Factory_setting
        | "10" -> Traumatized
        | _ -> assert false )
      , function
        | Shorty -> "1"
        | First_age -> "2"
        | Minuscule -> "3"
        | Mute -> "4"
        | Handless -> "5"
        | Phobic Phoby.Darkness -> "61"
        | Phobic Phoby.Attic -> "62"
        | Phobic Phoby.Basement -> "63"
        | Phobic Phoby.Isolation -> "64"
        | Phobic Phoby.Claustrophobic -> "65"
        | Phobic Phoby.That_one_kid -> "66"
        | Phobic Phoby.Adults -> "67"
        | Phobic Phoby.Scary_places -> "68"
        | Phobic Phoby.Insects -> "69"
        | Phobic Phoby.Water -> "610"
        | Phobic Phoby.Outside -> "611"
        | Phobic Phoby.Thunder -> "612"
        | Batteries -> "7"
        | Prima_donna -> "8"
        | Factory_setting -> "9"
        | Traumatized -> "10"
        | _ -> assert false ) )
