type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list
(*       | EnvDebugStm *)

 and exp = IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp

type env = (id * int) list

(*
let rec fmt_env (env : env) : string=
  match env with
  | (id, value) :: rest ->
    let s = Printf.sprintf "%s -> %d,\n" id value in
    s ^ fmt_env rest
  | [] -> ""
*)

let prog : stm = 
  CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
        EseqExp(PrintStm[IdExp"a"; OpExp(IdExp"a", Minus, NumExp 1)],
          OpExp(NumExp 10, Times, IdExp"a"))),
      PrintStm[IdExp "b"]))

let _ = Div

let rec maxargs (stm : stm) : int =
  match stm with
    | CompoundStm (a, b) -> max (maxargs a) (maxargs b)
    | AssignStm (_, e) -> helper e
    | PrintStm args ->
        let lst = (List.length args) :: (List.map helper args) in
          List.fold_left max (List.hd lst) lst
(*  | EnvDebugStm -> 0 *)
and helper (exp : exp) : int =
  match exp with
    | OpExp (a, _, b) -> max (helper a) (helper b)
    | EseqExp (s, e) -> max (maxargs s) (helper e)
    | _ -> 0


let rec interp (stm : stm) : unit =
  let _ = interp_stm [] stm in
  ()

and interp_stm (env : env) (stm : stm) : env =
  match stm with
  | CompoundStm (a, b) ->
    let env1 = interp_stm env a in
    interp_stm env1 b

  | AssignStm (id, e) ->
    let v, env1 = interp_exp env e in
    (id, v) :: env1

  | PrintStm arg ->
    let helper env e =
      let (value, env1) = interp_exp env e in
      let _ = print_int value in
      env1
    in
    List.fold_left helper env arg
  
(*| EnvDebugStm ->
    env |> fmt_env |> print_string;
    env *)

and interp_exp (env : env) (exp : exp) : int * env =
  match exp with
  | IdExp id ->
    let binding = List.find (fun pair -> fst pair = id) env in
    (snd binding, env)

  | NumExp x -> (x, env)

  | OpExp (e1, op, e2) ->
    let (v1, env1) = interp_exp env e1 in
    let (v2, env2) = interp_exp env1 e2 in
    let result = match op with
    | Plus -> v1 + v2
    | Minus -> v1 - v2
    | Times -> v1 * v2
    | Div -> v1 / v2 in
    (result, env2)

  | EseqExp (stm, e) ->
    let env1 = interp_stm env stm in
    interp_exp env1 e

let () =
  let _ = maxargs prog in ();
  interp prog;