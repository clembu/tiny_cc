module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html
open Tiny

let catalog_list ~title ~display_one ~describe ~build_state ~patch l =
  let display_one_li one =
    let build_state = build_state one in
    let build_button =
      let contents_s =
        React.S.map
          (function
            | `Added -> [ Icon.check () ]
            | `Can_add -> [ Icon.plus () ]
            | `Cannot_add -> [ Icon.cross () ] )
          build_state
      in
      let disabled_s =
        React.S.map
          (function `Added | `Can_add -> false | `Cannot_add -> true)
          build_state
      in
      let action_s =
        React.S.map ~eq:( == )
          (function
            | `Added -> fun () -> patch (Patch.Remove one)
            | `Can_add -> fun () -> patch (Patch.Add one)
            | `Cannot_add -> fun () -> () )
          build_state
      in
      let style_s =
        React.S.map
          (function
            | `Added -> `Primary | `Can_add -> `Normal | `Cannot_add -> `Error
            )
          build_state
      in
      Inputs.button_r ~style_s ~disabled_s ~action_s contents_s
    in
    F.li
      ~a:[ F.a_class [ "level"; "is-mobile" ] ]
      [ F.div ~a:[ F.a_class [ "level-left" ] ] [ F.txt @@ display_one one ]
      ; F.div
          ~a:[ F.a_class [ "level-right"; "buttons" ] ]
          [ build_button
          ; Inputs.button
              ~action:(fun _ ->
                Popup.open_popup
                @@ Popup.Box
                     [ F.div
                         ~a:[ F.a_class [ "block" ] ]
                         [ F.p
                             ~a:[ F.a_class [ "title" ] ]
                             [ F.txt @@ display_one one ]
                         ; F.article
                             ~a:[ F.a_class [ "content" ] ]
                             (describe one)
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

let flaws_catalog (type k) ~(kind : k Kind.t) ~patch ~build_s () =
  Layout.tile_box
    [ catalog_list ~title:"Flaws" ~display_one:Flaw.display
        ~build_state:(fun f ->
          React.S.map ~eq:( == ) (fun b -> State.flaw_build_state b f) build_s
          )
        ~describe:Descr.Flaw.show ~patch
      @@ Flaw.options kind
    ]

let powers_catalog (type k) ~(kind : k Kind.t) ~patch ~build_s () =
  Layout.tile_box
    [ catalog_list ~title:"Powers" ~display_one:Power.display
        ~build_state:(fun p ->
          React.S.map ~eq:( == )
            (fun b -> State.power_build_state kind b p)
            build_s )
        ~describe:Descr.Power.show ~patch
      @@ Power.options kind
    ]

let skill_group_bonus_input (type k g) ~(kind : k Tiny.Kind.t)
    ~(group : g Tiny.Skill.group) ~build_state ~value_s ~patch () =
  let name =
    match kind with
    | Tiny.Kind.Pet -> "pet-char-group-bonus-" ^ Tiny.Skill.group_name group
    | Tiny.Kind.Toy -> "toy-char-name" ^ Tiny.Skill.group_name group
  in
  let max_value = Build.max_group_bonus kind in
  let mk_button i =
    let disabled_s =
      React.S.map
        (function `Can_change -> false | `Cannot_change -> true)
        (build_state i)
    in
    let checked_s = React.S.map (fun v -> v >= i) value_s in
    let style_s =
      React.S.map
        (fun checked -> if checked then `Primary else `Normal)
        checked_s
    in
    let action_s = React.S.const (fun () -> patch i) in
    Inputs.button_r ~rounded:true ~style_s ~disabled_s ~action_s
      (React.S.const [ F.txt (string_of_int i) ])
  in
  let buttons = List.map mk_button @@ List.init (max_value + 1) Fun.id in
  F.div
    ~a:[ F.a_class [ "field" ] ]
    [ F.label ~a:[ F.a_class [ "label" ]; F.a_label_for name ] [ F.txt "Bonus" ]
    ; F.div
        ~a:[ F.a_class [ "control" ] ]
        [ F.div ~a:[ F.a_class [ "buttons" ] ] buttons ]
    ]

let skill_group_catalog (type k g) ~(kind : k Kind.t) ~(group : g Skill.group)
    ~patch ~build_s () =
  Layout.tile_box
    [ F.p
        ~a:[ F.a_class [ "title"; "is-3" ] ]
        [ F.txt (Skill.group_name group); F.txt " skills" ]
    ; skill_group_bonus_input ~kind ~group
        ~build_state:(fun i ->
          React.S.map ~eq:( == )
            (fun b ->
              let additional_cost = i - Build.group_bonus group b in
              State.skill_group_build_state kind b group additional_cost )
            build_s )
        ~value_s:(React.S.map (Build.group_bonus group) build_s)
        ~patch:(fun p -> patch (Patch.Set_bonus p))
        ()
    ; catalog_list ~title:"General skills" ~display_one:Skill.display
        ~build_state:(fun s ->
          React.S.map ~eq:( == )
            (fun b -> State.skill_build_state kind b group Skill.Gen s)
            build_s )
        ~describe:Descr.Skill.show
        ~patch:(fun p -> patch Patch.(Type (Gen p)))
      @@ Skill.options group Skill.Gen
    ; catalog_list ~title:"Special skills" ~display_one:Skill.display
        ~build_state:(fun s ->
          React.S.map ~eq:( == )
            (fun b -> State.skill_build_state kind b group Skill.Spec s)
            build_s )
        ~describe:Descr.Skill.show
        ~patch:(fun p -> patch Patch.(Type (Spec p)))
      @@ Skill.options group Skill.Spec
    ]

let skills_catalog ~kind ~patch ~build_s () =
  let mk group =
    skill_group_catalog ~kind ~group
      ~patch:(fun p -> patch (Patch.group_patch group p))
      ~build_s ()
  in
  let open Skill in
  Layout.v
    [ Layout.h [ mk Agile; Layout.v [ mk Strong; mk Tough; mk Fast ] ]
    ; Layout.h [ mk Smart; Layout.v [ mk Perceptive; mk Cute ] ]
    ]

let display (type k) ~(kind : k Kind.t) ~patch ~build_s () =
  F.div
    ~a:[ F.a_class [ "container" ] ]
    [ F.div
        ~a:[ F.a_class [ "section" ] ]
        [ F.div
            ~a:[ F.a_class [ "container" ] ]
            [ F.h1 ~a:[ F.a_class [ "title" ] ] [ F.txt "Catalog" ] ]
        ]
    ; F.section
        ~a:[ F.a_class [ "container" ] ]
        [ F.div
            ~a:[ F.a_class [ "tile"; "is-ancestor"; "is-vertical" ] ]
            [ Layout.h
                [ flaws_catalog ~kind
                    ~patch:(fun p -> patch (Patch.Flaw p))
                    ~build_s ()
                ; powers_catalog ~kind
                    ~patch:(fun p -> patch (Patch.Power p))
                    ~build_s ()
                ]
            ; skills_catalog ~kind
                ~patch:(fun p -> patch (Patch.Skills p))
                ~build_s ()
            ]
        ]
    ]
