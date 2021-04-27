module Kind = struct
  type pet (* = Kind_pet *)

  type toy (* = Kind_toy *)

  type _ t = Pet : pet t | Toy : toy t
end

module Power = struct
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

  let cp : type k. k t -> int = function
    | Pretty -> 2
    | Strength -> 1
    | Dream -> 2
    | Intimidation -> 1
    | Flight -> 2
    | Claws -> 1
    | Leap -> 1
    | HumanBFF -> 2
    | Voice_module -> 1
    | Accessories -> 1
    | Imaginary_friend -> 1
    | Authority -> 2
    | Elite_skill -> 1
    | Construction -> 4
    | Growth -> 2
    | Teddy -> 2
    | Odd_shape -> 1
    | Gigantism -> 2
    | Healing -> 2
    | Smarts -> 1
    | Licensed -> 1
    | Motorized -> 1
    | Multiplicity -> 2
    | Punching_ball -> 1
    | Slinky -> 1
    | Tough -> 2
    | Transformation -> 1
    | Malleable -> 2

  let options : type k. k Kind.t -> k t list = function
    | Kind.Pet ->
        [ Dream; Strength; Pretty; Flight; Intimidation; Claws; Leap; HumanBFF ]
    | Kind.Toy ->
        [ Dream
        ; Strength
        ; Pretty
        ; Flight
        ; Intimidation
        ; Voice_module
        ; Accessories
        ; Imaginary_friend
        ; Authority
        ; Elite_skill
        ; Construction
        ; Growth
        ; Teddy
        ; Odd_shape
        ; Gigantism
        ; Healing
        ; Smarts
        ; Licensed
        ; Motorized
        ; Multiplicity
        ; Punching_ball
        ; Slinky
        ; Tough
        ; Transformation
        ; Malleable
        ]

  let display : type k. k t -> string = function
    | Dream -> "Dream"
    | Strength -> "Strength"
    | Pretty -> "Pretty"
    | Flight -> "Flight"
    | Intimidation -> "Intimidation"
    | Claws -> "Claws/Fangs"
    | Leap -> "Leap"
    | HumanBFF -> "Human's Best Friend"
    | Voice_module -> "Voice Module"
    | Accessories -> "Accessories"
    | Imaginary_friend -> "Imaginary Friend"
    | Authority -> "Authority"
    | Elite_skill -> "Elite Skill"
    | Construction -> "Construction"
    | Growth -> "Growth"
    | Teddy -> "Teddy"
    | Odd_shape -> "Odd Shape"
    | Gigantism -> "Gigantism"
    | Healing -> "Healing"
    | Smarts -> "Smarts"
    | Licensed -> "Licensed"
    | Motorized -> "Motorized"
    | Multiplicity -> "Multiplicity"
    | Punching_ball -> "Punching Ball"
    | Slinky -> "Slinky"
    | Tough -> "Tough"
    | Transformation -> "Transformation"
    | Malleable -> "Malleable"

  let describe :
      type k.
      k t -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
      =
    let open Typo in
    function _ -> [ t "Hello "; b "World"; t "!" ]
end

module Skill = struct
  module G = struct
    type cute

    type agile

    type perceptive

    type strong

    type tough

    type smart

    type fast
  end

  module S = struct
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

  let group_name : type g. g group -> string = function
    | Cute -> "Cute"
    | Agile -> "Agile"
    | Perceptive -> "Perceptive"
    | Strong -> "Strong"
    | Tough -> "Tough"
    | Smart -> "Smart"
    | Fast -> "Fast"

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

  let display : type g s. (g, s) t -> string = function
    | Melee_wpn -> "Melee weapons"
    | Ranged_wpn -> "Ranged weapons"
    | Thrown_wpn -> "Thrown weapons"
    | Unarmed_combat -> "Unarmed combat"
    | Stealth -> "Stealth"
    | Balance -> "Balance"
    | Riding -> "Riding"
    | Dodging -> "Dodging"
    | Climbing -> "Climbing"
    | Handling -> "Handling"
    | Aerial -> "Aerial manoeuvres"
    | Acrobatics -> "Acrobatics"
    | War_machine -> "War machines"
    | KnockOut -> "KnockOut"
    | Driving_land -> "Driving Land Vehicles"
    | Counterfeit -> "Counterfeit"
    | Explosives -> "Explosives"
    | Swimming -> "Swimming"
    | Parrying -> "Parrying"
    | Pocketpicking -> "Pocketpicking"
    | Piloting_air -> "Piloting Air Vehicles"
    | Piloting_sea -> "Piloting Sea Vehicles"
    | Repair -> "Repairing"
    | Locksmith -> "Locksmithing"
    | Aiming -> "Aiming"
    | Biology -> "Biology"
    | Chemistry -> "Chemistry"
    | Deduction -> "Deduction"
    | Electricity -> "Electricity"
    | Town -> "Town"
    | Etiquette -> "Etiquette"
    | History -> "History"
    | Mecanics -> "Mecanics"
    | Memorizing -> "Memorizing"
    | Religion -> "Religion"
    | Remembering -> "Remembering"
    | Care_toy -> "Toy Care"
    | Crafting c -> "Crafting: " ^ c
    | Foreign_lang l -> "Language: " ^ l
    | Law -> "Law"
    | Economics -> "Economics"
    | Electronics -> "Electronics"
    | CS -> "Computer Science"
    | Firefighting -> "Firefighting"
    | Pharmacology -> "Pharmacology"
    | Care_human -> "Human Care"
    | Athleticism -> "Athleticism"
    | Smash -> "Smash"
    | Lift -> "Lift"
    | Stamina -> "Stamina"
    | Threaten -> "Threaten"
    | Cold_blood -> "Cold Blood"
    | Willpower -> "Willpower"
    | Torture_res -> "Resistance to Torture"
    | Search -> "Search"
    | Listen -> "Listen"
    | Nose -> "Nose"
    | Barter -> "Barter"
    | Navigation -> "Navigation"
    | Strategy -> "Strategy"
    | Tracking -> "Tracking"
    | Box_knowledge -> "Box knowledge"
    | Psychology -> "Psychology"
    | Tailing -> "Tailing"
    | Survival -> "Survival"
    | Charm -> "Charm"
    | Bluff -> "Bluff"
    | Relations -> "Relations"
    | Seduction -> "Seduction"
    | Speech -> "Speech"
    | Box_politics -> "Box politics"
    | Run -> "Run"
    | Reaction -> "Reaction"
    | Sprint -> "Sprint"
    | Act_first -> "Act first"

  let options : type g s. g group -> s ty -> (g, s) t list = function
    | Cute -> (
        function
        | Gen -> [ Charm; Bluff; Relations; Seduction ]
        | Spec -> [ Speech; Box_politics ] )
    | Agile -> (
        function
        | Gen ->
            [ Melee_wpn
            ; Ranged_wpn
            ; Thrown_wpn
            ; Unarmed_combat
            ; Stealth
            ; Balance
            ; Riding
            ; Dodging
            ; Climbing
            ; Handling
            ; Aerial
            ]
        | Spec ->
            [ Acrobatics
            ; War_machine
            ; KnockOut
            ; Driving_land
            ; Counterfeit
            ; Explosives
            ; Swimming
            ; Parrying
            ; Pocketpicking
            ; Piloting_air
            ; Piloting_sea
            ; Repair
            ; Locksmith
            ; Aiming
            ] )
    | Perceptive -> (
        function
        | Gen ->
            [ Search; Listen; Nose; Barter; Navigation; Strategy; Tracking ]
        | Spec -> [ Box_knowledge; Psychology; Tailing; Survival ] )
    | Strong -> ( function Gen -> [ Athleticism; Smash ] | Spec -> [ Lift ] )
    | Tough -> (
        function
        | Gen -> [ Stamina; Threaten; Cold_blood; Willpower ]
        | Spec -> [ Torture_res ] )
    | Smart -> (
        function
        | Gen ->
            [ Biology
            ; Chemistry
            ; Deduction
            ; Electricity
            ; Town
            ; Etiquette
            ; History
            ; Mecanics
            ; Memorizing
            ; Religion
            ; Remembering
            ; Care_toy
            ]
        | Spec ->
            [ Crafting ""
            ; Foreign_lang ""
            ; Law
            ; Economics
            ; Electronics
            ; CS
            ; Firefighting
            ; Pharmacology
            ; Care_human
            ] )
    | Fast -> (
        function Gen -> [ Run; Reaction; Sprint ] | Spec -> [ Act_first ] )

  let describe :
      type g s.
         (g, s) t
      -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list =
    let open Typo in
    function
    | Ranged_wpn ->
        [ p
            [ t "This skill allows to use all kinds of "
            ; b "ranged weapons"
            ; t ", real or dreamt. It also allows for "
            ; b "maintenance"
            ; t " and "
            ; b "repair"
            ; t " (might need materials or tools.)"
            ]
        ; p
            [ t "All ranged weapons follow the same "
            ; b "ammunitions management"
            ; t " rule:"
            ; F.br ()
            ; t "on every shot attempt, the player must first "
            ; t "succeed an "
            ; b "ammunition roll"
            ; t " (detailed in each weapon's description) by rolling "
            ; t "for a lower total than the weapon's value. "
            ; t "This system bundles under a single roll "
            ; t "things like weapon fiability, ammunition reliability, "
            ; t "and the possibility to run out of ammo. "
            ; t "A failed roll means an empty magazine or a jammed weapon, "
            ; t "which could be re-engaged with a new "
            ; b (display Ranged_wpn)
            ; t " roll and may need some tools."
            ]
        ; p
            [ t "Automatic weapons are handled with the "
            ; b (display War_machine)
            ; t " skill."
            ]
        ]
    | Melee_wpn ->
        [ p
            [ t "This skill allows to attack with all "
            ; b "melee weapons"
            ; t ", real or dreamt. It also allows for "
            ; b "maintenance"
            ; t " and "
            ; b "repair"
            ; t " (might need materials or tools.)"
            ]
        ]
    | Thrown_wpn ->
        [ p
            [ t "This skill allows to attack with all "
            ; b "thrown weapons"
            ; t " like stones, bolas, knives, grenades"
            ; t ", real or dreamt. It also allows for "
            ; b "maintenance"
            ; t " and "
            ; b "repair"
            ; t " (might need materials or tools.)"
            ]
        ]
    | Unarmed_combat ->
        [ p
            [ t "This skill allows to "
            ; b "strike a foe"
            ; t " efficiently without any weapon. It "
            ; b "does not increase damage"
            ; t " dealt without weapon, "
            ; t "it simply represents the character's ability "
            ; t "to hit properly without hurting themselves."
            ]
        ]
    | Stealth ->
        [ p
            [ t "This skill allows to "
            ; t "move around"
            ; t " and "
            ; t "handle loud objects "
            ; b "without making any noise. "
            ; t "Some detection systems "
            ; t "(such as a directional microphone or a sonar)"
            ; t " could still detect the character, though."
            ]
        ; p
            [ t "This skill also allows to "
            ; b "hide"
            ; t ", "
            ; b "move unseen"
            ; t ", or "
            ; b "hide an object"
            ; t " from someone."
            ]
        ]
    | Balance ->
        [ p
            [ t "This skill allows to "
            ; b "keep one's balance"
            ; t " when the situation requires it."
            ]
        ]
    | Riding ->
        [ p
            [ t "This skill allows to "
            ; b "ride any creature or toy"
            ; t " and keep it under control."
            ; t " Any non-trivial action like jumping, combat, or galopping"
            ; t " will require a "
            ; t (display Riding)
            ; t " roll."
            ]
        ; p
            [ t "This skill also allows for "
            ; b "knowledge"
            ; t " and "
            ; b "caring"
            ; t " of common domestic animals"
            ]
        ]
    | Dodging ->
        [ p
            [ t "This skill allows to "
            ; b "avoid an attack"
            ; t " without diving to the ground,"
            ; t " and to be able to act the following turn."
            ; t " A successful "
            ; t (display Dodging)
            ; t " roll completely negates the incoming attack."
            ]
        ; p [ t "The die to roll depends on the attack attempted:" ]
        ; ls
            [ [ t "A "; b "melee attack"; t " is dodged on a "; b "D6" ]
            ; [ t "A "; b "thrown attack"; t " is dodged on a "; b "D8" ]
            ; [ t "A "
              ; b "short-range ranged attack"
              ; t " is dodged on a "
              ; b "D10"
              ]
            ; [ t "A "
              ; b "long-range ranged attack"
              ; t " is dodged on a "
              ; b "D8"
              ]
            ]
        ]
    | Climbing ->
        [ p
            [ t "This skill allows to "
            ; b "climb"
            ; t " with or without special gear."
            ]
        ]
    | Handling ->
        [ p
            [ t "This skill allows to "
            ; b "activate"
            ; t ", "
            ; b "feel"
            ; t ", "
            ; b "handle"
            ; t ", and "
            ; b "examine"
            ; t " objects and surfaces."
            ; t "The character will have to make a "
            ; t (display Handling)
            ; t " roll to:"
            ]
        ; ls
            [ [ b "Activate"; t " an unknown mechanism" ]
            ; [ b "Find"; t " a secret button" ]
            ; [ b "Juggle" ]
            ; [ t "Do a magic trick" ]
            ; [ b "Feel around"; t " for a hidden door" ]
            ]
        ]
    | Aerial ->
        [ p
            [ t "This skill allows to efficiently control the power "
            ; b (Power.display Power.Flight)
            ; t ", for all the complex manoeuvres like"
            ; t " taking off, landing, dodging in mid-air, etc."
            ]
        ]
    | Acrobatics ->
        [ p [ t "This skill mostly allows to "; b "limit fall damage"; t "." ]
        ; p [ t "It also allows to perform extraordinary moves such as:" ]
        ; ls
            [ [ t "a "; b "cartwheel" ]
            ; [ b "contortions" ]
            ; [ t "keeping a "; b "flow"; t "of movement" ]
            ]
        ; p
            [ t "A character suffers 3 points of damage per"
            ; t " multiple of their scale factor."
            ; t " A successful "
            ; t (display Acrobatics)
            ; t " roll will cancel 3 of those points."
            ]
        ]
    | War_machine ->
        [ p
            [ t "This skill allows to attack with any and all "
            ; b "automatic weapons"
            ; t " as well as "
            ; b "canons"
            ; t " or "
            ; b "rocket launchers"
            ; t ". It also allows for their "
            ; b "repair"
            ; t " and "
            ; b "maintenance"
            ; t " (might need materials or tools.)"
            ]
        ; p
            [ t "Every weapon used by this skill fires in 3 different modes:"
            ; t " Single Fire, Bursts, or Full Auto."
            ]
        ; p
            [ t "A "
            ; b "full auto"
            ; t " attack focused on a single target deals "
            ; b "+10"
            ; t " damage. It can also be spread to a "
            ; b "90° field"
            ; t ", in which case the attack allows for up to "
            ; b "3 distincts damage rolls"
            ; t " on 3 different targets."
            ]
        ; p
            [ t "A "
            ; b "burst"
            ; t " shot only boosts the damage by "
            ; b "+5"
            ; t "."
            ]
        ]
    | KnockOut ->
        [ p
            [ t "This skill allows to "
            ; b "knock out a foe"
            ; t " for a duration of "
            ; b "1D6"
            ; t " rounds if they fail a "
            ; b (display Willpower)
            ; t " roll."
            ]
        ]
    | Driving_land ->
        [ p
            [ t "This skill allows to "
            ; b "drive"
            ; t "any and all land vehicles, "
            ; t "real or dreamt. It also allows for "
            ; b "maintenance"
            ; t ", but not for repair (that much is handled on a case-by-case"
            ; t " basis, depending on the nature of the vehicle: Electronics,"
            ; t " Mechanics, etc.)"
            ]
        ; p
            [ t "A character can drive a vehicle with a successful "
            ; t (display Driving_land)
            ; t " roll, up to a scale difference allowed by"
            ; t " the Interactions table."
            ; t " Beyond that, it'll be impossible because"
            ; t " the vehicle is too small, or"
            ; t " a group effort because it is too big."
            ]
        ; p
            [ t "Driving an actual real-life car will require 5 toys at least."
            ]
        ]
    | _ -> []
end

module Flaw = struct
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

  let describe :
      type k.
      k t -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
      = function
    | _ -> []
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

let pet = Pet Kind.Pet

let toy = Toy Kind.Toy

type 'k char_build =
  { name : string
  ; powers : 'k Power.t list
  ; flaws : 'k Flaw.t list
  ; skills : char_skills
  }

let new_build _ =
  { name = ""
  ; powers = []
  ; flaws = []
  ; skills =
      { smart_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Smart
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      ; agile_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Agile
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      ; strong_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Strong
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      ; tough_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Tough
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      ; fast_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Fast
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      ; cute_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Cute
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      ; perceptive_skills =
          { skill_group_bonus = 0
          ; skill_group = Skill.Perceptive
          ; skill_gen_skills = []
          ; skill_spec_skills = []
          }
      }
  }

module Patch = struct
  type 'a toggle = Add of 'a | Remove of 'a

  type 'g skill_ty =
    | Gen of ('g, Skill.S.g) Skill.t toggle
    | Spec of ('g, Skill.S.s) Skill.t toggle

  type 'g group = Set_bonus of int | Type of 'g skill_ty

  let apply_toggle l = function
    | Add e -> if List.mem e l then l else e :: l
    | Remove e -> List.filter (Fun.negate (( = ) e)) l

  let apply_group (type g) (g : g skill_group) = function
    | Set_bonus n -> { g with skill_group_bonus = n }
    | Type (Gen tg) ->
        { g with skill_gen_skills = apply_toggle g.skill_gen_skills tg }
    | Type (Spec tg) ->
        { g with skill_spec_skills = apply_toggle g.skill_spec_skills tg }

  type 'k flaw = 'k Flaw.t toggle

  let apply_flaw (type k) (b : k char_build) tg =
    { b with flaws = apply_toggle b.flaws tg }

  type 'k power = 'k Power.t toggle

  let apply_power (type k) (b : k char_build) tg =
    { b with powers = apply_toggle b.powers tg }

  type skills =
    | Agile of Skill.G.agile group
    | Cute of Skill.G.cute group
    | Perceptive of Skill.G.perceptive group
    | Strong of Skill.G.strong group
    | Tough of Skill.G.tough group
    | Fast of Skill.G.fast group
    | Smart of Skill.G.smart group

  let group_patch : type g. g Skill.group -> g group -> skills = function
    | Skill.Agile -> fun x -> Agile x
    | Skill.Cute -> fun x -> Cute x
    | Skill.Perceptive -> fun x -> Perceptive x
    | Skill.Fast -> fun x -> Fast x
    | Skill.Tough -> fun x -> Tough x
    | Skill.Smart -> fun x -> Smart x
    | Skill.Strong -> fun x -> Strong x

  let apply_skills s = function
    | Agile g -> { s with agile_skills = apply_group s.agile_skills g }
    | Cute g -> { s with cute_skills = apply_group s.cute_skills g }
    | Perceptive g ->
        { s with perceptive_skills = apply_group s.perceptive_skills g }
    | Smart g -> { s with smart_skills = apply_group s.smart_skills g }
    | Tough g -> { s with tough_skills = apply_group s.tough_skills g }
    | Strong g -> { s with strong_skills = apply_group s.strong_skills g }
    | Fast g -> { s with fast_skills = apply_group s.fast_skills g }

  type 'k t = Flaw of 'k flaw | Power of 'k power | Skills of skills

  let apply (type k) (b : k char_build) = function
    | Flaw f -> apply_flaw b f
    | Power p -> apply_power b p
    | Skills s -> { b with skills = apply_skills b.skills s }
end

let new_pet () = new_build Kind.Pet

let new_toy () = new_build Kind.Toy

type build_state = [ `Can_add | `Cannot_add | `Added ]

let valid_power : type k. k Kind.t -> k char_build -> k Power.t -> bool =
 fun k b p ->
  match k with
  | Kind.Pet ->
      if List.mem Flaw.Harmless b.flaws then p <> Power.Claws else true
  | Kind.Toy -> true

let valid_skill :
    type k g s.
       k Kind.t
    -> k char_build
    -> g Skill.group
    -> s Skill.ty
    -> (g, s) Skill.t
    -> bool =
 fun k b g _t _s ->
  match k with
  | Kind.Toy ->
      if List.mem Flaw.First_age b.flaws then
        match g with Skill.Smart -> false | _ -> true
      else true
  | Kind.Pet -> true

let skills_group_cost :
    type k g. k Kind.t -> k char_build -> g skill_group -> int =
 fun k b g ->
  let gen_cost =
    List.length
      (List.filter (valid_skill k b g.skill_group Skill.Gen) g.skill_gen_skills)
  in
  if gen_cost = 0 then 0
  else
    gen_cost
    + 2
      * List.length
          (List.filter
             (valid_skill k b g.skill_group Skill.Spec)
             g.skill_spec_skills )

let skills_cost : type k. k Kind.t -> k char_build -> int =
 fun k b ->
  skills_group_cost k b b.skills.fast_skills
  + skills_group_cost k b b.skills.cute_skills
  + skills_group_cost k b b.skills.smart_skills
  + skills_group_cost k b b.skills.tough_skills
  + skills_group_cost k b b.skills.strong_skills
  + skills_group_cost k b b.skills.perceptive_skills
  + skills_group_cost k b b.skills.agile_skills

let available_cp : type k. k Kind.t -> k char_build -> int =
 fun k b ->
  let bonus = List.fold_left ( + ) 0 @@ List.map Flaw.cp b.flaws in
  let pool = 20 + bonus in
  let pool =
    List.fold_left ( - ) pool @@ List.map Power.cp
    @@ List.filter (valid_power k b) b.powers
  in
  pool - skills_cost k b

let flaw_build_state : type k. k char_build -> k Flaw.t -> build_state =
 fun b f ->
  if List.mem f b.flaws then `Added
  else
    match b.flaws with
    | [] -> `Can_add
    | _ :: _ :: _ -> `Cannot_add
    | _ -> `Can_add

let power_build_state :
    type k. k Kind.t -> k char_build -> k Power.t -> build_state =
 fun k b p ->
  if List.mem p b.powers then `Added
  else
    let current_power_cost =
      List.fold_left ( + ) 0 @@ List.map Power.cp b.powers
    in
    let av_cp =
      min (available_cp k b)
        ((match k with Kind.Pet -> 3 | Kind.Toy -> 4) - current_power_cost)
    in
    if av_cp <= 0 then `Cannot_add
    else if valid_power k b p then `Can_add
    else `Cannot_add

let skill_build_state :
    type k g s.
       k Kind.t
    -> k char_build
    -> g Skill.group
    -> s Skill.ty
    -> (g, s) Skill.t
    -> build_state =
 fun k b g t s ->
  let group_has_skill g =
    match t with
    | Skill.Gen -> List.mem s g.skill_gen_skills
    | Skill.Spec -> List.mem s g.skill_spec_skills
  in
  let group_has_gen_skill () =
    match g with
    | Skill.Agile -> List.length b.skills.agile_skills.skill_gen_skills > 0
    | Skill.Perceptive ->
        List.length b.skills.perceptive_skills.skill_gen_skills > 0
    | Skill.Cute -> List.length b.skills.cute_skills.skill_gen_skills > 0
    | Skill.Fast -> List.length b.skills.fast_skills.skill_gen_skills > 0
    | Skill.Tough -> List.length b.skills.tough_skills.skill_gen_skills > 0
    | Skill.Strong -> List.length b.skills.strong_skills.skill_gen_skills > 0
    | Skill.Smart -> List.length b.skills.smart_skills.skill_gen_skills > 0
  in
  let has_skill =
    match g with
    | Skill.Agile -> group_has_skill b.skills.agile_skills
    | Skill.Perceptive -> group_has_skill b.skills.perceptive_skills
    | Skill.Cute -> group_has_skill b.skills.cute_skills
    | Skill.Fast -> group_has_skill b.skills.fast_skills
    | Skill.Tough -> group_has_skill b.skills.tough_skills
    | Skill.Strong -> group_has_skill b.skills.strong_skills
    | Skill.Smart -> group_has_skill b.skills.smart_skills
  in
  if has_skill then `Added
  else if available_cp k b <= 0 then `Cannot_add
  else
    match t with
    | Skill.Gen -> `Can_add
    | Skill.Spec -> if group_has_gen_skill () then `Can_add else `Cannot_add
