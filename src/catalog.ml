module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let catalog_list ~title ~display_one ~describe l =
  let display_one_li one =
    F.li
      ~a:[ F.a_class [ "level"; "is-mobile" ] ]
      [ F.div ~a:[ F.a_class [ "level-left" ] ] [ F.txt @@ display_one one ]
      ; F.div
          ~a:[ F.a_class [ "level-right" ] ]
          [ Inputs.button
              ~action:(fun _ ->
                Popup.open_popup
                  [ F.div
                      ~a:[ F.a_class [ "block" ] ]
                      [ F.p
                          ~a:[ F.a_class [ "title" ] ]
                          [ F.txt @@ display_one one ]
                      ; F.article (describe one)
                      ]
                  ]
                ; false )
              [ Icon.question () ]
          ]
      ]
  in
  F.div
    ~a:[ F.a_class [ "block" ] ]
    [ F.p ~a:[ F.a_class [ "title"; "is-5" ] ] [ F.txt title ]
    ; F.ul @@ List.map display_one_li l
    ]

let flaws_catalog (type k) ~(kind : k Char_ty.Kind.t) () =
  Layout.tile_box
    [ catalog_list ~title:"Flaws" ~display_one:Char_ty.Flaw.display
        ~describe:(Fun.const [ F.span [ F.txt "hello" ] ])
      @@ Char_ty.Flaw.options kind
    ]

let powers_catalog (type k) ~(kind : k Char_ty.Kind.t) () =
  Layout.tile_box
    [ catalog_list ~title:"Powers" ~display_one:Char_ty.Power.display
        ~describe:(Fun.const [ F.span [ F.txt "hello" ] ])
      @@ Char_ty.Power.options kind
    ]

let skill_group_catalog (type g) ~(group : g Char_ty.Skill.group) () =
  Layout.tile_box
    [ F.p
        ~a:[ F.a_class [ "title"; "is-3" ] ]
        [ F.txt (Char_ty.Skill.group_name group); F.txt " skills" ]
    ; catalog_list ~title:"General skills" ~display_one:Char_ty.Skill.display
        ~describe:Char_ty.Skill.describe
      @@ Char_ty.Skill.options group Char_ty.Skill.Gen
    ; catalog_list ~title:"Special skills" ~display_one:Char_ty.Skill.display
        ~describe:Char_ty.Skill.describe
      @@ Char_ty.Skill.options group Char_ty.Skill.Spec
    ]

let skills_catalog () =
  let mk group = skill_group_catalog ~group () in
  let open Char_ty.Skill in
  Layout.v
    [ Layout.h [ mk Agile; Layout.v [ mk Strong; mk Tough; mk Fast ] ]
    ; Layout.h [ mk Smart; Layout.v [ mk Perceptive; mk Cute ] ]
    ]

let display (type k) ~(kind : k Char_ty.Kind.t) () =
  F.div
    ~a:[ F.a_class [ "container" ] ]
    [ F.nav
        ~a:[ F.a_class [ "navbar" ] ]
        [ F.div
            ~a:[ F.a_class [ "container" ] ]
            [ F.h1 ~a:[ F.a_class [ "title" ] ] [ F.txt "Catalog" ] ]
        ]
    ; F.section
        ~a:[ F.a_class [ "container" ] ]
        [ F.div
            ~a:[ F.a_class [ "tile"; "is-ancestor"; "is-vertical" ] ]
            [ Layout.h [ flaws_catalog ~kind (); powers_catalog ~kind () ]
            ; skills_catalog ()
            ]
        ]
    ]
