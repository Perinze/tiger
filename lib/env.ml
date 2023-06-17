type ty = Types.ty

type enventry =
| VarEntry of {ty : ty}
| FunEntry of {formals : ty list; result : ty}
| DummyEntry

let bind s ty = (Symbol.symbol s, ty)
let f = fun tbl (s, t) -> Symbol.enter s t tbl

let base_tenv : ty Symbol.table =
  List.fold_left f Symbol.empty
    [ bind "int" Types.INT;
      bind "string" Types.STRING ]

let base_venv : enventry Symbol.table =
  let g (s, f, r) = bind s (FunEntry {formals=f; result=r}) in
  List.fold_left f Symbol.empty
    (List.map g
      [ ("print", [Types.STRING], Types.UNIT);
        ("flush", [], Types.UNIT);
        ("getchar", [], Types.STRING);
        ("ord", [Types.STRING], Types.INT);
        ("chr", [Types.INT], Types.STRING);
        ("size", [Types.STRING], Types.INT);
        ("substring", [Types.STRING; Types.INT; Types.INT], Types.STRING);
        ("concat", [Types.STRING; Types.STRING], Types.STRING);
        ("not", [Types.INT], Types.INT);
        ("exit", [Types.INT], Types.UNIT);
      ])