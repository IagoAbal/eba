
exception Not_implemented

let not_implemented () = raise Not_implemented

exception Panic of string

let panic () = raise (Panic("fatal error"))

let panic_with msg = raise (Panic(msg))
