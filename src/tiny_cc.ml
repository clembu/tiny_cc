module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R
module RF = R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let name_input (type k) ?(init = `Default "") ~(kind : k Tiny.Kind.t) () =
  let name =
    match kind with
    | Tiny.Kind.Pet -> "pet-char-name"
    | Tiny.Kind.Toy -> "toy-char-name"
  in
  F.div
    ~a:[ F.a_class [ "field" ] ]
    [ F.label ~a:[ F.a_class [ "label" ]; F.a_label_for name ] [ F.txt "Name" ]
    ; F.div
        ~a:[ F.a_class [ "control" ] ]
        [ Inputs.text_input ~value:init ~placeholder:"Ticklebums" () ]
    ]

let identity_tile (type k) ~(kind : k Tiny.Kind.t) ?name () =
  F.div
    ~a:[ F.a_class [ "tile"; "is-parent" ] ]
    [ F.div
        ~a:[ F.a_class [ "tile"; "is-child"; "box" ] ]
        [ name_input ?init:name ~kind () ]
    ]

let counter ?size ?label value_s =
  let size_class = Option.map (fun s -> "is-" ^ string_of_int s) size in
  F.div
    ~a:[ F.a_class [ "level-item"; "has-text-centered" ] ]
    [ F.div
        ( Option.map
            (fun label -> F.p ~a:[ F.a_class [ "heading" ] ] [ F.txt label ])
            label
        @? [ F.p
               ~a:[ F.a_class (size_class @? [ "title" ]) ]
               [ RF.txt @@ React.S.map string_of_int value_s ]
           ] )
    ]

let editable_list ?title ?roll ?options ~patch ~display_one ~codec l_s =
  let add_one one = patch (Tiny.Patch.Add one) in
  let delete one = patch (Tiny.Patch.Remove one) in
  let display_one_li one =
    F.li
      ~a:[ F.a_class [ "level"; "is-mobile" ] ]
      [ F.div ~a:[ F.a_class [ "level-left" ] ] [ F.txt @@ display_one one ]
      ; F.div
          ~a:[ F.a_class [ "level-right" ] ]
          [ F.button
              ~a:
                [ F.a_onclick (fun _ ->
                      delete one
                      ; false )
                ; F.a_class [ "delete" ]
                ]
              []
          ]
      ]
  in
  let add_elt_o =
    let roll_btn_o =
      Option.map
        (fun roll ->
          Inputs.button
            ~action:(fun _ ->
              add_one (roll ())
              ; false )
            [ F.txt "Roll for it" ] )
        roll
    in
    let add_select_o =
      Option.map (Inputs.select ~on_change:add_one ~codec ~display_one) options
    in
    if roll_btn_o = None && add_select_o = None then None
    else
      Some
        (F.div
           ~a:[ F.a_class [ "block" ] ]
           [ F.p ~a:[ F.a_class [ "heading" ] ] [ F.txt "Add one" ]
           ; F.div
               ~a:[ F.a_class [ "level" ] ]
               ( Option.map
                   (fun add_select ->
                     F.div ~a:[ F.a_class [ "level-left" ] ] [ add_select ] )
                   add_select_o
               @? Option.map
                    (fun roll_btn ->
                      F.div ~a:[ F.a_class [ "level-right" ] ] [ roll_btn ] )
                    roll_btn_o
               @? [] )
           ] )
  in
  F.div
    ~a:[ F.a_class [ "block" ] ]
    ( Option.map
        (fun title -> F.p ~a:[ F.a_class [ "title"; "is-5" ] ] [ F.txt title ])
        title
    @? ( RF.ul ~a:[ F.a_class [ "block" ] ]
       @@ ReactiveData.RList.map display_one_li
       @@ ReactiveData.RList.from_signal l_s )
    @: add_elt_o @? [] )

let flaws_list (type k) ~patch ~(kind : k Tiny.Kind.t) rl =
  editable_list ~title:"Flaws"
    ~roll:(fun () -> Tiny.Flaw.roll kind)
    ~patch ~display_one:Tiny.Flaw.display ~codec:(Tiny.Flaw.codec kind) rl

let creation_points_tile (type k) ~(kind : k Tiny.Kind.t) ~patch ~flaws_s cp_s =
  F.div
    ~a:[ F.a_class [ "tile"; "is-parent" ] ]
    [ F.div
        ~a:[ F.a_class [ "tile"; "is-child"; "box" ] ]
        [ F.div
            ~a:[ F.a_class [ "level" ] ]
            [ counter ~label:"Creation points" cp_s ]
        ; flaws_list ~patch ~kind flaws_s
        ]
    ]

let char_builder (type k) ~(kind : k Tiny.Kind.t) () =
  let ((name_s, _) as name_r) = React.S.create "" in
  let patch_e, patch = React.E.create () in
  let build_s =
    React.S.fold Tiny.Patch.apply (Tiny.Build.new_build kind) patch_e
  in
  let cp_s = React.S.map (Tiny.State.available_cp kind) build_s in
  [ F.nav
      ~a:[ F.a_class [ "navbar"; "is-fixed-top"; "is-spaced" ] ]
      [ F.div
          ~a:[ F.a_class [ "container" ] ]
          [ F.div ~a:[ F.a_class [ "navbar-item" ] ] [ counter ~size:5 cp_s ]
          ; F.h1
              ~a:[ F.a_class [ "title" ] ]
              [ F.txt "Character creation"
              ; RF.txt
                @@ React.S.map
                     (fun name -> if name = "" then "" else ": " ^ name)
                     name_s
              ]
          ]
      ]
  ; F.section
      ~a:[ F.a_class [ "section" ] ]
      [ F.div
          ~a:[ F.a_class [ "container" ] ]
          [ F.div
              ~a:[ F.a_class [ "tile"; "is-ancestor" ] ]
              [ identity_tile ~kind ~name:(`S name_r) ()
              ; creation_points_tile ~kind
                  ~patch:(fun p -> patch (Tiny.Patch.Flaw p))
                  ~flaws_s:(React.S.map (fun b -> b.Tiny.Build.flaws) build_s)
                  cp_s
              ]
          ]
      ]
  ; F.section
      ~a:[ F.a_class [ "section" ] ]
      [ Catalog.display ~kind ~patch ~build_s () ]
  ]

let kind_selector ~set_kind () =
  F.section
    ~a:[ F.a_class [ "hero"; "is-fullheight" ] ]
    [ F.div
        ~a:[ F.a_class [ "hero-body" ] ]
        [ F.div
            ~a:[ F.a_class [ "container"; "has-text-centered" ] ]
            [ F.h1 ~a:[ F.a_class [ "title" ] ] [ F.txt "Character creation" ]
            ; F.h2
                ~a:[ F.a_class [ "subtitle" ] ]
                [ F.txt "What kind of character are we making?" ]
            ; F.div
                ~a:[ F.a_class [ "buttons"; "is-centered" ] ]
                [ Inputs.button
                    ~action:(fun _ ->
                      set_kind (Some Tiny.Build.toy)
                      ; false )
                    [ Icon.toy (); F.span [ F.txt "Toy" ] ]
                ; Inputs.button
                    ~action:(fun _ ->
                      set_kind (Some Tiny.Build.pet)
                      ; false )
                    [ Icon.pet (); F.span [ F.txt "Pet" ] ]
                ]
            ]
        ]
    ]

let display ~popups () =
  let char_kind_s, set_kind = React.S.create None in
  let char_creator =
    React.S.map
      (function
        | None -> [ kind_selector ~set_kind () ]
        | Some (Tiny.Build.Pet kind) -> char_builder ~kind ()
        | Some (Tiny.Build.Toy kind) -> char_builder ~kind () )
      char_kind_s
  in
  let body = React.S.l2 ( @ ) char_creator popups in
  RF.body
    ~a:[ F.a_class [ "has-navbar-fixed-top" ] ]
    (ReactiveData.RList.from_signal body)

let () =
  J.Dom_html.window##.onload :=
    J.Dom_html.handler @@ fun _ ->
    let popups =
      React.E.select
        [ React.E.map (fun c -> `Open c) Popup.open_popup_e
        ; React.E.map (fun () -> `Close) Popup.close_popup_e
        ]
      |> React.S.fold
           (fun l e ->
             match (l, e) with
             | [], `Close -> []
             | _ :: tl, `Close -> tl
             | l, `Open c -> l @ [ Popup.display c ] )
           []
      |> React.S.trace (function
           | [] ->
               J.Dom_html.document##.documentElement##.classList##remove
                 (J.Js.string "is-clipped")
           | _ ->
               J.Dom_html.document##.documentElement##.classList##add
                 (J.Js.string "is-clipped") )
    in
    Js_of_ocaml_tyxml.Tyxml_js.Register.html (display ~popups ())
    ; J.Js._false
