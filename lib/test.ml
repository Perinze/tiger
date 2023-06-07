open Base

let parse (prog: string) =
  Parzer.parse Lexer.token
    (Lexing.from_string prog)

let parse_success (prog: string) =
  try let _ = parse prog in true
  with _ -> false

(* DECLARATIONS *)

(* DATA TYPES *)

let%test "parser tydec id" =
  parse_success
    "let type a = b in end"

let%test "parser tydec record empty" =
  parse_success
    "let type a = {} in end"

let%test "parser tydec record 1 field" =
  parse_success
    "let type a = {b: int} in end"

let%test "parser tydec record 3 fields" =
  parse_success
    "let type a = {b: int, c: string, d: e} in end"

let%test "parser tydec array" =
  parse_success
    "let type a = array of int in end"

let%test "parser mutually recursive types" =
  parse_success
    "let
      type intlist = {hd: int, tl: intlist}
      type tree = {key: int, children: treelist}
     in end"

(* VARIABLES *)

let%test "parser vardec without type" =
  parse_success
    "let var a := 6 in end"

let%test "parser vardec with type" =
  parse_success
    "let var a: int := 6 in end"

(* FUNCTIONS *)

let%test "parser fundec procedure" =
  parse_success
    "let function f() = 6 in end"

let%test "parser fundec procedure with 1 parameter" =
  parse_success
    "let function f(x: int) = x in end"

let%test "parser fundec procedure with 3 parameter" =
  parse_success
    "let function f(x: int, y: string, z: d) = y in end"

let%test "parser fundec function with 2 parameter" =
  parse_success
    "let function g(x: int, y: t): int = x in end"

(* VARIABLES AND EXPRESSIONS *)

(* L-VALUES *)

let%test "parser lvalue id" =
  parse_success
    "a"

let%test "parser lvalue field of record" =
  parse_success
    "a.b"

let%test "parser lvalue field of nested record" =
  parse_success
    "a.b.c.d"

let%test "parser lvalue element of array" =
  parse_success
    "a[6]"

let%test "parser lvalue element of 3 dimension array" =
  parse_success
    "a[6][7][8]"

let%test "parser lvalue mixed" =
  parse_success
    "a.b[7].c.d[1][2].e"

let%test "parser nil" =
  parse_success
    "nil"

let%test "parser sequencing" =
  parse_success
    "(6; (); a.b; c[7]; a)"

let%test "parser unit" =
  parse_success
    "()"

let%test "parser let decs in end" =
  parse_success
    "let var a := b in end"

let%test "parser integer literal" =
  parse_success
    "6"

let%test "string literal" =
  parse_success
    "\"ok\""

let%test "parser negation" =
  parse_success
    "-6"
  
let%test "parser function call" =
  parse_success
    "f()"

let%test "parser function call with 1 parameter" =
  parse_success
    "g(6)"

let%test "parser function call with 3 parameters" =
  parse_success
    "h(6, a, -1)"

let%test "parser arithmetic" =
  parse_success
    "1 + 2 * 3 / 4"

let%test "parser comparison" =
  parse_success
    "1 = 2 <> 3 > 4 < 5 >= 6 <= 7"

let%test "parser boolean operators" =
  parse_success
    "2 & 0 | 1"

let%test "parser record creation" =
  parse_success
    "a {}"

let%test "parser record creation with 1 field" =
  parse_success
    "a {b = 1}"

let%test "parser record creation with 3 fields" =
  parse_success
    "a {b = 1, c = d.e, f = g[0]}"

let%test "parser array creation" =
  parse_success
    "a [16] of 6"

let%test "parser extent" =
  parse_success
    "a {b = c [16] of 6,
        j = d {e = f {g = h [1] of 0},
           i = k}}"

let%test "parser assignment to id" =
  parse_success
    "a := 6"

let%test "parser assignment to record field" =
  parse_success
    "a.b := 6"

let%test "parser assignment to array element" =
  parse_success
    "a[6] := 6"

let%test "parser assignment to compound lvalue" =
  parse_success
    "a.b[c][f].d[0].e := 6"

let%test "parser if then else" =
  parse_success
    "if a then b else c"

let%test "parser if then" =
  parse_success
    "if a then 6"

let%test "parser while" =
  parse_success
    "while a do b"

let%test "parser for" =
  parse_success
    "for a := 0 to b do 6"

let%test "parser break" =
  parse_success
    "while a do (b; break)"

let%test "parser let decs in exp end" =
  parse_success
    "let type a = b var c: a := d in c end"

let%test "parser parentheses" =
  parse_success
    "(1 + 2) * 3"

let%test "parser queens.tig" =
  parse_success "
let
  var N := 8

  type intArray = array of int

  var row := intArray [ N ] of 0
  var col := intArray [ N ] of 0
  var diag1 := intArray [N+N-1] of 0
  var diag2 := intArray [N+N-1] of 0

  function printboard() = (
    for i := 0 to N-1 do (
      for j := 0 to N-1 do
        print(if col[i]=j then \" O\" else \" .\");
      print(\"\\n\")
    );
    print(\"\\n\")
  )

  function try(c: int) =
    if c = N then
      printboard()
    else
      for r := 0 to N-1 do
        if row[r] = 0 & diag1[r+c] = 0 & diag2[r+7-c] = 0 then (
          row[r] := 1; diag1[r+c] := 1; diag2[r+7-c] := 1;
          col[c] := r;
          try(c+1);
          row[r] := 0; diag1[r+c] := 0; diag2[r+7-c] := 0
        )
  
  in try(0)
end"

let%test "parser merge.tig" =
  parse_success "
let
  type any = {any: int}
  var buffer := getchar()
  
  function readint(any: any): int =
    let
      var i := 0
      function isdigit(s: string): int =
        ord(buffer) >= ord(\"0\") & ord(buffer) <= ord(\"9\")
    in
    end

  type list = {first: int, rest: list}

  function readlist(): list =
    let
      var any := any {any = 0}
      var i := readint(any)
    in
      if any.any
        then list {first = i, rest = readlist()}
        else (buffer := getchar(); nil)
    end

  function merge(a: list, b: list): list =
    if a = nil then b
    else if b = nil then a
    else if a.first < b.first
      then list {first = a.first, rest = merge(a.rest, b)}
      else list {first = b.first, rest = merge(a, b.rest)}
  
  function printint(i: int) =
    let
      function f(i: int) =
        if i > 0 then (
          f(i / 10);
          print(chr(i - i / 10 * 10 + ord(\"0\")))
        )
    in
      if i < 0 then (print(\"-\"); f(-i))
      else if i > 0 then f(i)
      else print(\"0\")
    end
  
  function printlist(l: list) =
    if l = nil then print(\"\\n\")
    else (printint(l.first); print(\" \"); printlist(l.rest))

in printlist(merge(readlist(), readlist()))
end
"