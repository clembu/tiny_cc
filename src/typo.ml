module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let t s = F.span [ F.txt s ]

let b s = F.span ~a:[ F.a_class [ "has-text-weight-bold" ] ] [ F.txt s ]

let i s = F.span ~a:[ F.a_class [ "is-italic" ] ] [ F.txt s ]

let p t = F.p ~a:[ F.a_class [ "block" ] ] t

let li s = F.li s

let ls l = F.ul @@ List.map li l
