type symbol = string * int
module StringIntOrd : Map.OrderedType = struct
  type t = string * int
  let compare (_, a) (_, b) = compare a b
end

module H = Hashtbl

exception Symbol
let nextsym = ref 0
let size_hint = 128
let hashtable : (string, int) H.t
  = H.create size_hint

let symbol name =
  match H.find_opt hashtable name with
  | Some i -> (name, i)
  | None -> let i = !nextsym
      in nextsym := i+1;
    H.add hashtable name i;
    (name, i)

let name (s, _) = s

module Table : Map.S
with type key = StringIntOrd.t
= Map.Make(StringIntOrd)

type 'a table = 'a Table.t
let empty = Table.empty
let add = Table.add
let find = Table.find