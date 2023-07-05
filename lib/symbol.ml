type symbol = string * int [@@deriving show]

module H = Hashtbl

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

module Table = Table.IntMapTable(struct
  type key = symbol
  let getInt (_, n) = n
end)

type 'a table = 'a Table.table
let empty = Table.empty
let enter = Table.enter
let look = Table.look