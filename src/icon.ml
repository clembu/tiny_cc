module F = Js_of_ocaml_tyxml.Tyxml_js.Html

type 'a icon = unit -> 'a F.elt

let wrap fa = F.span ~a:[ F.a_class [ "icon" ] ] [ F.i ~a:[ F.a_class fa ] [] ]

let toy () = wrap [ "fas"; "fa-robot" ]

let pet () = wrap [ "fas"; "fa-paw" ]

let question () = wrap [ "fas"; "fa-question-circle" ]
