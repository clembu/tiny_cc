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
      | Gen -> [ Search; Listen; Nose; Barter; Navigation; Strategy; Tracking ]
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
