module T = Tyxml.Html

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch
  ; s

let print_html style script outf =
  let style = Option.map read_file style in
  let script = Option.map read_file script in
  let outfile = open_out @@ Option.value ~default:"out.html" outf in
  let fmt = Format.formatter_of_out_channel outfile in
  T.pp () fmt
  @@ T.html
       (T.head
          (T.title (T.txt "TINY Character Creation"))
          (T.meta
             ~a:
               [ T.a_name "viewport"
               ; T.a_content "width=device-width, initial-scale=1"
               ]
             ()
           ::
           T.meta ~a:[ T.a_charset "UTF-8" ] ()
           ::
           T.script
             ~a:
               [ T.a_src "https://kit.fontawesome.com/4a094e7c9c.js"
               ; T.a_crossorigin `Anonymous
               ]
             (T.cdata_script "")
           ::
           T.link ~rel:[ `Stylesheet ]
             ~href:
               "https://fonts.googleapis.com/css?family=Nunito+Sans:200,300,400,600,700"
             ()
           ::
           T.link ~rel:[ `Stylesheet ]
             ~href:"https://fonts.googleapis.com/css?family=Montserrat:400,700"
             ()
           ::
           List.filter_map Fun.id
             [ Option.map
                 (fun style ->
                   T.style
                     ~a:[ T.a_mime_type "text/css" ]
                     [ T.cdata_style style ] )
                 style
             ; Option.map
                 (fun script -> T.script (T.cdata_script script))
                 script
             ] ) )
       (T.body [])
  ; close_out outfile

open Cmdliner

let style_file =
  Arg.(
    value & opt (some non_dir_file) None & info [ "style" ] ~docv:"STYLE_FILE")

let script_file =
  Arg.(
    value & opt (some non_dir_file) None & info [ "script" ] ~docv:"SCRIPT_FILE")

let out_file =
  Arg.(
    value & opt (some string) None & info [ "output"; "o" ] ~docv:"OUTPUT_FILE")

let cmd =
  let doc = "Generate html file from given script and stylesheet" in
  ( Term.(const print_html $ style_file $ script_file $ out_file)
  , Term.info "gen_html" ~version:"v1.0.0" ~doc ~exits:Term.default_exits )

let () = Term.(exit @@ eval cmd)
