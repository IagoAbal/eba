
module SP = SmartPrint

type doc = SmartPrint.t

let (!^) = SP.(!^)

let space = SP.space

let newline = SP.newline

let comma = SP.(!^ ",")

let colon = SP.(!^ ":")

let (+) = SP.(^-^)

let (++) = SP.(^^)

let int = SP.OCaml.int

let parens = SP.parens

let braces = SP.braces

let brackets = SP.brakets

let separate = SP.separate

let space_sep = separate space

let comma_sep = separate comma

let indent = SP.indent

let to_string = SP.to_string 70 4
