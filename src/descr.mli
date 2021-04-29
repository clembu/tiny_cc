module Skill : sig
  val show :
       ('c, 's) Tiny.Skill.t
    -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
end

module Power : sig
  val show :
       'k Tiny.Power.t
    -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
end

module Flaw : sig
  val show :
       'k Tiny.Flaw.t
    -> Html_types.article_content Js_of_ocaml_tyxml.Tyxml_js.Html.elt list
end
