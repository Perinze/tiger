open Base

let parse (prog: string) =
  Parzer.parse Lexer.token
    (Lexing.from_string prog)

let parse_produce_unit (prog: string) =
  equal_unit () (parse prog)

(* DECLARATIONS *)

(* DATA TYPES *)

let%test "parser tydec id" =
  parse_produce_unit
    "let type a = b in end"

let%test "parser tydec record empty" =
  parse_produce_unit
    "let type a = {} in end"

let%test "parser tydec record 1 field" =
  parse_produce_unit
    "let type a = {b: int} in end"

let%test "parser tydec record 3 fields" =
  parse_produce_unit
    "let type a = {b: int, c: string, d: e} in end"

let%test "parser tydec array" =
  parse_produce_unit
    "let type a = array of int in end"

(* VARIABLES *)

let%test "parser vardec without type" =
  parse_produce_unit
    "let var a := 6 in end"

let%test "parser vardec with type" =
  parse_produce_unit
    "let var a: int := 6 in end"

(* FUNCTIONS *)

let%test "parser fundec procedure" =
  parse_produce_unit
    "let function f() = 6 in end"

let%test "parser fundec procedure with 1 parameter" =
  parse_produce_unit
    "let function f(x: int) = x in end"

let%test "parser fundec procedure with 3 parameter" =
  parse_produce_unit
    "let function f(x: int, y: string, z: d) = y in end"

let%test "parser fundec function with 2 parameter" =
  parse_produce_unit
    "let function g(x: int, y: t): int = x in end"

(* VARIABLES AND EXPRESSIONS *)

(* L-VALUES *)

let%test "parser lvalue id" =
  parse_produce_unit
    "a"

let%test "parser lvalue field of record" =
  parse_produce_unit
    "a.b"

let%test "parser lvalue element of array" =
  parse_produce_unit
    "a[6]"

let%test "parser test nil" =
  parse_produce_unit "nil"
