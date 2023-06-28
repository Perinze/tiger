val any_errors : bool ref
exception Error
val error : Lexing.position -> string -> unit
val impossible : string -> unit (* raises Error *)