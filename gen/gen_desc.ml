open Core
module P = Parsetree
module A = Asttypes
module L = Location
module O = Omd

let mk_exp desc =
  { P.pexp_desc = desc
  ; pexp_loc = L.none
  ; pexp_loc_stack = []
  ; pexp_attributes = []
  }

let mk_typ desc =
  { P.ptyp_desc = desc
  ; ptyp_loc = L.none
  ; ptyp_loc_stack = []
  ; ptyp_attributes = []
  }

(* does not reverse the list *)
let rec mk_list = function
  | [] ->
      mk_exp
        (P.Pexp_construct
           ({ A.loc = L.none; A.txt = Longident.Lident "[]" }, None) )
  | h :: l ->
      mk_exp
        (P.Pexp_construct
           ( { A.loc = L.none; A.txt = Longident.Lident "::" }
           , Some (mk_exp (P.Pexp_tuple [ h; mk_list l ])) ) )

let rec inline_to_rawstr il =
  match il.O.il_desc with
  | O.Concat l -> List.fold_left (fun s x -> s ^ inline_to_rawstr x) "" l
  | O.Text s | O.Code s | O.Html s -> s
  | O.Emph i
  | O.Strong i
  | O.Link { O.label = i; _ }
  | O.Image { O.label = i; _ } ->
      inline_to_rawstr i
  | O.Hard_break | O.Soft_break -> ""

let ( <<$ ) f s = f @@ Lexing.from_string s

let to_ls_apply f lexp =
  mk_exp
    (P.Pexp_apply
       ( mk_exp
           (P.Pexp_ident
              { A.loc = L.none
              ; A.txt = Parse.val_ident <<$ Printf.sprintf "F.%s" f
              } )
       , [ (A.Nolabel, lexp) ] ) )

let rec inline_to_ml { O.il_desc = il; _ } =
  match il with
  | O.Concat _ -> assert false
  | O.Text s -> Some (Parse.expression <<$ Printf.sprintf "F.txt \"%s\"" s)
  | O.Emph il -> Option.some @@ to_ls_apply "em" @@ inline_to_ml_wrap il
  | O.Strong il -> Option.some @@ to_ls_apply "strong" @@ inline_to_ml_wrap il
  | O.Code s ->
      Option.some
      @@ to_ls_apply "code"
           (Parse.expression <<$ Printf.sprintf "[F.txt \"%s\"]" s)
  | O.Html _ -> None
  | O.Link { O.label; O.destination; _ } ->
      Option.some
      @@
      if destination = "" then to_ls_apply "span" @@ inline_to_ml_wrap label
      else
        Parse.expression
        <<$ Printf.sprintf "F.strong [F.txt (Tiny.%s.display Tiny.%s.%s)]"
              (inline_to_rawstr label) (inline_to_rawstr label) destination
  | O.Image _ -> None
  | O.Soft_break -> Some (Parse.expression <<$ "F.txt \" \"")
  | O.Hard_break -> Some (Parse.expression <<$ "F.br ()")

and inline_to_ml_wrap ({ O.il_desc = ild; _ } as il) =
  match ild with
  | O.Concat l -> mk_list @@ List.filter_map inline_to_ml l
  | _ -> mk_list @@ inline_to_ml il @? []

and block_to_ml = function
  | O.Paragraph il -> to_ls_apply "p" @@ inline_to_ml_wrap il
  | O.List (Bullet _, _, l) ->
      to_ls_apply "ul" @@ mk_list
      @@ List.map
           (fun bls ->
             to_ls_apply "li" @@ mk_list
             @@ List.map (fun b -> block_to_ml b.O.bl_desc) bls )
           l
  | O.List (Ordered _, _, l) ->
      to_ls_apply "ol" @@ mk_list
      @@ List.map
           (fun bls ->
             to_ls_apply "li" @@ mk_list
             @@ List.map (fun b -> block_to_ml b.O.bl_desc) bls )
           l
  | O.Blockquote l ->
      to_ls_apply "blockquote" @@ mk_list
      @@ List.map (fun b -> block_to_ml b.O.bl_desc) l
  | O.Thematic_break -> Parse.expression @@ Lexing.from_string @@ "F.hr ()"
  | O.Heading (n, il) ->
      to_ls_apply (Printf.sprintf "h%d" n) @@ inline_to_ml_wrap il
  | O.Code_block _ -> failwith "UNSUPPORTED CODEBLOCK"
  | O.Html_block _ -> failwith "UNSUPPORTED HTMLBLOCK"
  | O.Definition_list _ -> failwith "UNSUPPORTED DEFLIST"

let mk_case m (`Desc (s, md)) =
  { P.pc_lhs =
      { ppat_desc =
          P.Ppat_construct
            ( { A.loc = L.none
              ; A.txt = Option.get (Longident.unflatten [ "Tiny"; m; s ])
              }
            , None )
      ; ppat_loc = L.none
      ; ppat_loc_stack = []
      ; ppat_attributes = []
      }
  ; pc_guard = None
  ; pc_rhs = mk_list @@ List.rev_map block_to_ml @@ md
  }

let dummy_case () =
  { P.pc_lhs =
      { ppat_desc = P.Ppat_any
      ; ppat_loc = L.none
      ; ppat_loc_stack = []
      ; ppat_attributes = []
      }
  ; pc_guard = None
  ; pc_rhs = mk_list []
  }

let show_fun tyvars m descs =
  { P.pstr_desc =
      P.Pstr_value
        ( A.Nonrecursive
        , [ { pvb_pat =
                { ppat_desc = P.Ppat_var { A.loc = L.none; A.txt = "show" }
                ; ppat_loc = L.none
                ; ppat_loc_stack = []
                ; ppat_attributes = []
                }
            ; pvb_expr =
                List.fold_left
                  (fun exp tyvar ->
                    mk_exp
                      (P.Pexp_newtype ({ A.loc = L.none; A.txt = tyvar }, exp))
                    )
                  (mk_exp
                     (P.Pexp_constraint
                        ( mk_exp
                            (P.Pexp_function
                               (List.map (mk_case m) descs @ [ dummy_case () ])
                            )
                        , mk_typ
                            (P.Ptyp_arrow
                               ( A.Nolabel
                               , mk_typ
                                   (P.Ptyp_constr
                                      ( { A.loc = L.none
                                        ; A.txt =
                                            Parse.longident
                                            <<$ Printf.sprintf "Tiny.%s.t" m
                                        }
                                      , List.map
                                          (fun tyvar ->
                                            mk_typ
                                              (P.Ptyp_constr
                                                 ( { A.loc = L.none
                                                   ; A.txt =
                                                       Parse.longident <<$ tyvar
                                                   }
                                                 , [] ) ) )
                                          tyvars ) )
                               , Parse.core_type
                                 <<$ "Html_types.article_content \
                                      Js_of_ocaml_tyxml.Tyxml_js.Html.elt list"
                               ) ) ) ) )
                  tyvars
            ; pvb_attributes = []
            ; pvb_loc = L.none
            }
          ] )
  ; P.pstr_loc = L.none
  }

let to_module (`Mod (m, descs)) =
  { P.pstr_desc =
      Pstr_module
        { pmb_name = { A.loc = L.none; A.txt = Some m }
        ; pmb_expr =
            { pmod_desc =
                P.Pmod_structure
                  [ (let tyvars =
                       match m with "Skill" -> [ "c"; "s" ] | _ -> [ "k" ]
                     in
                     show_fun tyvars m descs )
                  ]
            ; pmod_loc = L.none
            ; pmod_attributes = []
            }
        ; pmb_attributes = []
        ; pmb_loc = L.none
        }
  ; pstr_loc = L.none
  }

let to_ml p =
  { P.pstr_desc =
      Pstr_module
        { pmb_name = { A.loc = L.none; A.txt = Some "F" }
        ; pmb_expr =
            { pmod_desc =
                Pmod_ident
                  { A.loc = L.none
                  ; A.txt =
                      Parse.simple_module_path
                      <<$ "Js_of_ocaml_tyxml.Tyxml_js.Html"
                  }
            ; pmod_loc = L.none
            ; pmod_attributes = []
            }
        ; pmb_attributes = []
        ; pmb_loc = L.none
        }
  ; pstr_loc = L.none
  }
  :: List.map to_module p

let rec parse p = function
  | [] -> p
  | { O.bl_desc = O.Heading (1, il); _ } :: l ->
      parse (`Mod (inline_to_rawstr il, []) :: p) l
  | { O.bl_desc = O.Heading (2, il); _ } :: l ->
      parse
        ( match p with
        | `Mod (m, d) :: p ->
            `Mod (m, `Desc (inline_to_rawstr il, []) :: d) :: p
        | _ -> assert false )
        l
  | { O.bl_desc = bl; _ } :: l ->
      parse
        ( match p with
        | `Mod (m, `Desc (s, desc) :: d) :: p ->
            `Mod (m, `Desc (s, bl :: desc) :: d) :: p
        | _ -> assert false )
        l

let to_ml_top l =
  let p = parse [] l in
  to_ml p

let read_file filename =
  let ch = open_in filename in
  let md = Omd.of_channel ch in
  close_in ch
  ; md

let print_ml md outf =
  let md = read_file md in
  let outfile = match outf with Some outf -> open_out outf | None -> stdout in
  let tree = to_ml_top md in
  output_string outfile (Pprintast.string_of_structure tree)

open Cmdliner

let md_file =
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"MD_FILE")

let out_file =
  Arg.(value & pos 1 (some string) None & info [] ~docv:"OUTPUT_FILE")

let cmd =
  let doc = "Generate ocaml file from given markdown " in
  ( Term.(const print_ml $ md_file $ out_file)
  , Term.info "gen_desc" ~version:"v1.0.0" ~doc ~exits:Term.default_exits )

let () = Term.(exit @@ eval cmd)
