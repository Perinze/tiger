type temp
val newtemp : unit -> temp
val makestring : temp -> string

type label = Symbol.symbol
val newlabel : unit -> label
val namedlabel : string -> label