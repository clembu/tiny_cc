module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let tile_box content =
  F.div
    ~a:[ F.a_class [ "tile"; "is-parent" ] ]
    [ F.div ~a:[ F.a_class [ "tile"; "is-child"; "box" ] ] content ]

let h tiles = F.div ~a:[ F.a_class [ "tile" ] ] tiles

let v tiles = F.div ~a:[ F.a_class [ "tile"; "is-vertical" ] ] tiles

let level ?(left = []) ?(right = []) l =
  F.div
    ~a:[ F.a_class [ "level" ] ]
    (F.div ~a:[ F.a_class [ "level-left" ] ] left
     ::
     F.div ~a:[ F.a_class [ "level-right" ] ] right
     :: List.map (fun cnt -> F.div ~a:[ F.a_class [ "level-item" ] ] cnt) l )
