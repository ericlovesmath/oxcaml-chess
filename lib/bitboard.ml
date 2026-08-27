module I = Stdlib_upstream_compatible.Int64_u

type t = int64#

let empty = #0L
let full = I.lognot empty
let of_square ~rank ~file = I.shift_left #1L ((rank * 8) + file)
let union a b = I.logor a b
let inter a b = I.logand a b
let mem t ~rank ~file = not (I.equal (inter t (of_square ~rank ~file)) empty)
