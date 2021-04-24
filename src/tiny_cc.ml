module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R
module RF = R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let name_input (type k) ?(init = `Default "") ~(kind : k Char_ty.Kind.t) () =
  let name =
    match kind with
    | Char_ty.Kind.Pet -> "pet-char-name"
    | Char_ty.Kind.Toy -> "toy-char-name"
  in
  F.div
    ~a:[ F.a_class [ "field" ] ]
    [ F.label ~a:[ F.a_class [ "label" ]; F.a_label_for name ] [ F.txt "Name" ]
    ; F.div
        ~a:[ F.a_class [ "control" ] ]
        [ Inputs.text_input ~value:init ~placeholder:"Ticklebums" () ]
    ]

let identity_tile (type k) ~(kind : k Char_ty.Kind.t) ?name () =
  F.div
    ~a:[ F.a_class [ "tile"; "is-parent" ] ]
    [ F.div
        ~a:[ F.a_class [ "tile"; "is-child"; "box" ] ]
        [ name_input ?init:name ~kind () ]
    ]

let counter ~label value_s =
  F.div
    ~a:[ F.a_class [ "level-item"; "has-text-centered" ] ]
    [ F.div
        [ F.p ~a:[ F.a_class [ "heading" ] ] [ F.txt label ]
        ; F.p
            ~a:[ F.a_class [ "title" ] ]
            [ RF.txt @@ React.S.map string_of_int value_s ]
        ]
    ]

let rec find_idx : type a. int -> a -> a list -> int =
 fun c x -> function
  | [] -> raise Not_found
  | hd :: tl -> if hd = x then c else find_idx (c + 1) x tl

let find_idx : type a. a -> a list -> int = fun x l -> find_idx 0 x l

let editable_list ?title ?roll ~options ~on_delete ~on_add ~display_one ~codec
    (l_r, l_h) =
  let add_one one =
    ReactiveData.RList.snoc one l_h
    ; on_add one
  in
  let delete one =
    let idx = ReactiveData.RList.value l_r |> find_idx one in
    ReactiveData.RList.remove idx l_h
    ; on_delete one
  in
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
  let add_elt =
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
    let add_select =
      Inputs.select ~on_change:add_one ~codec ~display_one options
    in
    F.div
      ~a:[ F.a_class [ "block" ] ]
      [ F.p ~a:[ F.a_class [ "heading" ] ] [ F.txt "Add one" ]
      ; F.div
          ~a:[ F.a_class [ "level" ] ]
          ( F.div ~a:[ F.a_class [ "level-left" ] ] [ add_select ]
          @: Option.map
               (fun roll_btn ->
                 F.div ~a:[ F.a_class [ "level-right" ] ] [ roll_btn ] )
               roll_btn_o
          @? [] )
      ]
  in
  F.div
    ~a:[ F.a_class [ "block" ] ]
    ( Option.map
        (fun title -> F.p ~a:[ F.a_class [ "title"; "is-5" ] ] [ F.txt title ])
        title
    @? ( RF.ul ~a:[ F.a_class [ "block" ] ]
       @@ ReactiveData.RList.map display_one_li l_r )
    @: add_elt @: [] )

let flaws_list (type k) ~add_cp ~(kind : k Char_ty.Kind.t) rl =
  let on_add flaw = add_cp (Char_ty.Flaw.cp flaw) in
  let on_delete flaw = add_cp (-Char_ty.Flaw.cp flaw) in
  let options = Char_ty.Flaw.options kind in
  editable_list ~title:"Flaws"
    ~roll:(fun () -> Char_ty.Flaw.roll kind)
    ~options ~on_delete ~on_add ~display_one:Char_ty.Flaw.display
    ~codec:(Char_ty.Flaw.codec kind) rl

let creation_points_tile (type k) ~(kind : k Char_ty.Kind.t)
    ((cp_s, set_cp) : int Reactlib.S.component) =
  let add_cp i = set_cp (React.S.value cp_s + i) in
  let flaws_r :
      k Char_ty.Flaw.t ReactiveData.RList.t
      * k Char_ty.Flaw.t ReactiveData.RList.handle =
    ReactiveData.RList.create []
  in
  F.div
    ~a:[ F.a_class [ "tile"; "is-parent" ] ]
    [ F.div
        ~a:[ F.a_class [ "tile"; "is-child"; "box" ] ]
        [ F.div
            ~a:[ F.a_class [ "level" ] ]
            [ counter ~label:"Creation points" cp_s ]
        ; flaws_list ~add_cp ~kind flaws_r
        ]
    ]

let char_builder (type k) ~(kind : k Char_ty.Kind.t) () =
  let ((name_s, _) as name_r) = React.S.create "" in
  let cp_r = React.S.create 20 in
  [ F.section
      ~a:[ F.a_class [ "section" ] ]
      [ F.nav
          ~a:[ F.a_class [ "navbar" ] ]
          [ F.div
              ~a:[ F.a_class [ "container" ] ]
              [ F.h1
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
          ~a:[ F.a_class [ "container" ] ]
          [ F.div
              ~a:[ F.a_class [ "tile"; "is-ancestor" ] ]
              [ identity_tile ~kind ~name:(`S name_r) ()
              ; creation_points_tile ~kind cp_r
              ]
          ]
      ]
  ; F.section ~a:[ F.a_class [ "section" ] ] [ Catalog.display ~kind () ]
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
                      set_kind (Some Char_ty.toy)
                      ; false )
                    [ Icon.toy (); F.span [ F.txt "Toy" ] ]
                ; Inputs.button
                    ~action:(fun _ ->
                      set_kind (Some Char_ty.pet)
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
        | Some (Char_ty.Pet kind) -> char_builder ~kind ()
        | Some (Char_ty.Toy kind) -> char_builder ~kind () )
      char_kind_s
  in
  let body = React.S.l2 ( @ ) char_creator popups in
  RF.body (ReactiveData.RList.from_signal body)

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
