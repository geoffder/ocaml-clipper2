(lang dune 3.6)

(name clipper)

(using directory-targets 0.1)

(using ctypes 0.1)

(generate_opam_files true)

(source
 (github geoffder/ocaml-clipper2))

(documentation https://geoffder.github.io/ocaml-clipper2)

(license GPL-2.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name clipper)
 (synopsis
  "OCaml bindings to the Clipper2 polygon clipping/offsetting library.")
 (description
  "OCaml bindings to the Clipper2 polygon clipping/offsetting library.")
 (depends
  (ocaml
   (>= 4.14.0))
  ctypes
  ctypes-foreign
  conf-cmake
  (ppx_inline_test :with-test)
  (OCADml
   (and
    (>= 0.3.2)
    :with-test))
  (gg :with-test)))
