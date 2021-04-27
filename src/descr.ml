open Typo
module T = Tiny
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

module Power = struct
  let show :
      type k.
         k T.Power.t
      -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list =
    function
    | _ -> [ t "Hello "; b "World"; t "!" ]
end

module Flaw = struct
  let show :
      type k.
         k T.Flaw.t
      -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list =
    function
    | _ -> []
end

module Skill = struct
  let show :
      type g s.
         (g, s) T.Skill.t
      -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list =
    let open T.Skill in
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
            ; b (T.Power.display T.Power.Flight)
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
