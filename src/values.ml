module R = Js_of_ocaml_tyxml.Tyxml_js.R
module RF = R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

type comparator = GT of int React.signal | LT of int React.signal

let counter ?size ?label ?compared_s value_s =
  let size_class = Option.map (fun s -> "is-" ^ string_of_int s) size in
  F.div
    ~a:[ F.a_class [ "level-item"; "has-text-centered" ] ]
    [ F.div
        ( Option.map
            (fun label -> F.p ~a:[ F.a_class [ "heading" ] ] [ F.txt label ])
            label
        @? [ (let base_classes = size_class @? [ "title" ] in
              let classes =
                match compared_s with
                | None -> F.a_class base_classes
                | Some (GT s) ->
                    RF.a_class
                      (React.S.l2
                         (fun v cmp ->
                           if v >= cmp then "has-text-success" :: base_classes
                           else "has-text-danger" :: base_classes )
                         value_s s )
                | Some (LT s) ->
                    RF.a_class
                      (React.S.l2
                         (fun v cmp ->
                           if v <= cmp then "has-text-success" :: base_classes
                           else "has-text-danger" :: base_classes )
                         value_s s )
              in
              F.p ~a:[ classes ]
                (RF.txt (React.S.map string_of_int value_s)
                 ::
                 ( match compared_s with
                 | None -> []
                 | Some (GT s) | Some (LT s) ->
                     [ F.txt " / "; RF.txt (React.S.map string_of_int s) ] ) )
             )
           ] )
    ]
