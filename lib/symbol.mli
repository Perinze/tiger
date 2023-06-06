type symbol

val symbol : string -> symbol
val name : symbol -> string

type 'a table

val empty : 'a table
val add : symbol -> 'a -> 'a table -> 'a table
val find : 'a table * symbol -> 'a option