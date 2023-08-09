type frame
type access

val new_frame : (name : Temp.label) -> (formals : bool list) -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val alloc_local : frame -> bool -> access