module J = Js_of_ocaml
module R = Js_of_ocaml_tyxml.Tyxml_js.R
module RF = R.Html
module F = Js_of_ocaml_tyxml.Tyxml_js.Html

let button ~action contents =
  F.button ~a:[ F.a_onclick action; F.a_class [ "button" ] ] contents

let button_r ?(rounded = false) ?(style_s = React.S.const `Normal)
    ?(disabled_s = React.S.const false) ~action_s contents_s =
  let class_s =
    style_s
    |> React.S.map (function
         | `Normal -> []
         | `Primary -> [ "is-primary" ]
         | `Error -> [ "is-danger" ] )
    |> React.S.map (fun l ->
           "button" :: (if rounded then "is-rounded" :: l else l) )
  in
  RF.button
    ~a:
      [ RF.a_onclick (fun _ ->
            React.S.value action_s ()
            ; false )
      ; R.filter_attrib (F.a_disabled ()) disabled_s
      ; RF.a_class class_s
      ]
    (ReactiveData.RList.from_signal contents_s)

let get_input_value_from_event e =
  J.Js.Opt.map
    (J.Js.Opt.bind e##.target (fun target -> J.Dom_html.CoerceTo.input target))
    (fun input -> input##.value)

let get_select_value_from_event e =
  J.Js.Opt.map
    (J.Js.Opt.bind e##.target (fun target -> J.Dom_html.CoerceTo.select target))
    (fun input -> input##.value)

let iter_value f e =
  J.Js.Opt.iter
    (J.Js.Opt.case
       (get_input_value_from_event e)
       (fun () ->
         J.Js.Opt.map (get_select_value_from_event e) (fun v ->
             J.Js.to_string v ) )
       (fun v -> J.Js.Opt.return @@ J.Js.to_string v) )
    f

let text_input ?name ?(value = `Default "") ?placeholder () =
  let value_s, set = Reactlib.init_signal value in
  F.input
    ~a:
      ( Option.map F.a_placeholder placeholder
      @? Option.map F.a_name name
      @? [ F.a_oninput (fun e ->
               iter_value set e
               ; true )
         ; F.a_value (React.S.value value_s)
         ; F.a_input_type `Text
         ; F.a_class [ "input" ]
         ] )
    ()

let select ?on_change ~codec:(of_json, to_json) ~display_one options =
  F.div
    ~a:
      ( F.a_class [ "select" ]
      @: Option.map
           (fun on_change ->
             F.a_onchange (fun e ->
                 iter_value (fun s -> on_change (of_json s)) e
                 ; false ) )
           on_change
      @? [] )
    [ F.select
        (List.map
           (fun one ->
             F.option ~a:[ F.a_value @@ to_json one ]
             @@ F.txt @@ display_one one )
           options )
    ]
