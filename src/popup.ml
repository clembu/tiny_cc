module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

type content =
  | Box of Html_types.div_content F.elt list
  | Card of
      { header : Html_types.div_content F.elt list
      ; body : Html_types.div_content F.elt list
      ; footer : Html_types.div_content F.elt list
      }

let (open_popup_e : content React.event), open_popup = React.E.create ()

let close_popup_e, close_popup = React.E.create ()

let box content =
  F.div
    ~a:[ F.a_class [ "modal-content" ] ]
    [ F.div ~a:[ F.a_class [ "box" ] ] content ]

let card ~header ~footer body =
  F.div
    ~a:[ F.a_class [ "modal-card" ] ]
    [ F.div ~a:[ F.a_class [ "modal-card-head" ] ] header
    ; F.div ~a:[ F.a_class [ "modal-card-body" ] ] body
    ; F.div ~a:[ F.a_class [ "modal-card-foot" ] ] footer
    ]

let display_content = function
  | Box content -> box content
  | Card { header; footer; body } -> card ~header ~footer body

let display content =
  F.div
    ~a:[ F.a_class [ "modal"; "is-active" ] ]
    [ F.div
        ~a:
          [ F.a_class [ "modal-background" ]
          ; F.a_onclick (fun _ ->
                close_popup ()
                ; false )
          ]
        []
    ; display_content content
    ; F.button
        ~a:
          [ F.a_class [ "modal-close" ]
          ; F.a_onclick (fun _ ->
                close_popup ()
                ; false )
          ]
        []
    ]
