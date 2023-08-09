type temp = int

let nexttemp = ref 0
let newtemp () =
  let t = !nexttemp in
  nexttemp := !nexttemp + 1;
  t

let makestring (temp : temp) =
  "t" ^ (string_of_int temp)


type label = Symbol.symbol

let numlabel = ref 0
let newlabel () =
  let name = "L" ^ (string_of_int !numlabel) in
  let l = Symbol.symbol name in
  numlabel := !numlabel + 1;
  l

let namedlabel name =
  Symbol.symbol name