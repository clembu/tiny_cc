module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let (open_popup_e : Html_types.div_content F.elt list React.event), open_popup =
  React.E.create ()

let close_popup_e, close_popup = React.E.create ()

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
    ; F.div
        ~a:[ F.a_class [ "modal-content" ] ]
        [ F.div ~a:[ F.a_class [ "box" ] ] content ]
    ; F.button
        ~a:
          [ F.a_class [ "modal-close" ]
          ; F.a_onclick (fun _ ->
                close_popup ()
                ; false )
          ]
        []
    ]
