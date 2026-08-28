(lang dune 3.19)

(name oxcaml-chess)

(generate_opam_files true)

(source
 (github ericlovesmath/oxcaml-chess))

(authors "Eric Lee")

(maintainers "Eric Lee")

(license MIT)

(package
 (name oxcaml-chess)
 (synopsis "A chess engine written in OxCaml")
 (description
  "A chess engine built on OxCaml's unboxed types and mode system")
 (depends
  (ocaml (>= 5.2))
  stdlib_upstream_compatible
  ocaml_intrinsics_kernel
  core))
