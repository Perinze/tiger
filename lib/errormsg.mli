val any_errors : bool ref
val filename : string ref
val line_num : int ref
val line_pos : int list ref
val error : int -> string -> unit
exception Error
val impossible : string -> 'a (* raises Error *)
val reset : unit -> unit