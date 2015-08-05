
exception Not_implemented of string

let not_implemented str = raise (Not_implemented str)

let hole () = not_implemented "<hole>"

exception Panic of string

let panic () = raise (Panic("fatal error"))

let panic_with msg = raise (Panic(msg))
