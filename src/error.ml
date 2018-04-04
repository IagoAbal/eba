
exception Not_implemented of string

let not_implemented str = raise (Not_implemented str)

let hole () = not_implemented "<hole>"

exception Panic of string

let panic () = raise (Panic("fatal error"))

(* TODO: Turn it into a `printf' style function. *)
let panic_with msg = raise (Panic(msg))
