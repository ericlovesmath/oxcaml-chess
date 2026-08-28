module I = Stdlib_upstream_compatible.Int64_u
module Intrinsics = Ocaml_intrinsics_kernel.Int64.Unboxed

type t = int64#

let empty = #0L
let full = I.lognot empty
let union a b = I.logor a b
let inter a b = I.logand a b
let complement a = I.lognot a
let diff a b = I.logand a (I.lognot b)
let equal a b = I.equal a b
let count t = I.to_int (Intrinsics.count_set_bits t)
