type 'a icon = unit -> 'a Js_of_ocaml_tyxml.Tyxml_js.Html.elt

val toy : [> `Span ] icon

val pet : [> `Span ] icon

val question : [> `Span ] icon

val check : [> `Span ] icon

val plus : [> `Span ] icon

val cross : [> `Span ] icon

val kind_icon : 'k Tiny.Kind.t -> [> `Span ] icon
