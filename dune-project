(lang dune 2.7)
(name tiny-cc)

(generate_opam_files true)

(source (github clembu/tiny_cc))
(license MIT)
(authors "Clément Busschaert")
(maintainers "clement.busschaert@gmail.com")

(package
 (name tiny-cc)
 (synopsis "Character creation helper for the TINY rpg")
 (description "Character creation helper for the TINY rpg")
 (depends
  (dune (> 1.5))
   js_of_ocaml
   js_of_ocaml-tyxml
   js_of_ocaml-ppx
   react
   tyxml
   cmdliner
   omd
  ))
