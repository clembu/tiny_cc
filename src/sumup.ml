module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html
open Tiny

let skill_group_sumup (type g) (g : g Build.skill_group) =
  let mk_one skill = F.li [ F.txt @@ Skill.display skill ] in
  F.section
    ~a:[ F.a_class [ "block" ] ]
    [ Layout.level
        ~left:
          [ F.h4
              ~a:[ F.a_class [ "title"; "is-4" ] ]
              [ F.txt @@ Skill.group_name g.skill_group ]
          ]
        ~right:
          [ Values.counter ~label:"Group bonus"
              (React.S.const g.skill_group_bonus)
          ]
        []
    ; F.div
        ~a:[ F.a_class [ "block" ] ]
        [ F.h6 ~a:[ F.a_class [ "title"; "is-6" ] ] [ F.txt "General skills" ]
        ; F.ul @@ List.map mk_one g.Build.skill_gen_skills
        ]
    ; F.div
        ~a:[ F.a_class [ "block" ] ]
        [ F.h6
            ~a:[ F.a_class [ "title"; "is-6" ] ]
            [ F.txt "Specialized skills" ]
        ; F.ul @@ List.map mk_one g.Build.skill_spec_skills
        ]
    ]

let skills_sumup (type k) (b : k Build.char_build) =
  F.section
    ~a:[ F.a_class [ "block" ] ]
    [ Layout.level
        ~left:[ F.h2 ~a:[ F.a_class [ "title"; "is-2" ] ] [ F.txt "Skills" ] ]
        ~right:
          [ Values.counter ~label:"CP Spent"
              ~compared_s:
                (Values.GT
                   (React.S.const (Build.min_skills_investment b.Build.kind)) )
              (React.S.const (State.skills_cost ~with_bonus:false b))
          ]
        []
    ; skill_group_sumup b.skills.agile_skills
    ; skill_group_sumup b.skills.tough_skills
    ; skill_group_sumup b.skills.strong_skills
    ; skill_group_sumup b.skills.smart_skills
    ; skill_group_sumup b.skills.perceptive_skills
    ; skill_group_sumup b.skills.cute_skills
    ; skill_group_sumup b.skills.fast_skills
    ]

let powers_sumup (type k) (b : k Build.char_build) =
  let mk_one pow = F.li [ F.txt @@ Power.display pow ] in
  F.section
    ~a:[ F.a_class [ "block" ] ]
    [ Layout.level
        ~left:[ F.h2 ~a:[ F.a_class [ "title"; "is-2" ] ] [ F.txt "Powers" ] ]
        ~right:
          [ Values.counter ~label:"CP Spent"
              ~compared_s:
                (Values.LT (React.S.const (Build.max_power_cost b.Build.kind)))
              (React.S.const (State.powers_cost b))
          ]
        []
    ; F.ul @@ List.map mk_one b.Build.powers
    ]

let flaws_sumup (type k) (b : k Build.char_build) =
  let mk_one flaw = F.li [ F.txt @@ Flaw.display flaw ] in
  F.section
    ~a:[ F.a_class [ "block" ] ]
    [ Layout.level
        ~left:[ F.h2 ~a:[ F.a_class [ "title"; "is-2" ] ] [ F.txt "Flaws" ] ]
        ~right:
          [ Values.counter ~label:"CP Bonus"
              (React.S.const (State.flaws_bonus_cp b))
          ]
        []
    ; F.ul @@ List.map mk_one b.Build.flaws
    ]

let display (type k) ~name (b : k Build.char_build) =
  let header =
    [ F.p
        ~a:[ F.a_class [ "modal-card-title" ] ]
        [ F.txt (if name = "" then "Nameless character" else name) ]
    ; F.p
        ~a:[ F.a_class [ "title"; "is-5" ] ]
        [ Icon.kind_icon b.Build.kind ()
        ; F.txt " "
        ; F.txt (String.capitalize_ascii (Kind.display b.Build.kind))
        ]
    ]
  in
  let footer = [] in
  let body = [ flaws_sumup b; powers_sumup b; skills_sumup b ] in
  Popup.Card { header; footer; body }
