module A = Absyn
module E = Env
module S = Symbol
exception NotImplemented

type venv = E.enventry S.table
type tenv = Types.ty S.table

module Translate = struct
  type exp = unit
end

type expty = {exp : Translate.exp; ty : Types.ty}

let trans_prog (_ : A.exp) : unit =
  ()

let check_int {exp=_; ty=ty} pos =
  match ty with
  | Types.INT -> ()
  | _ -> Errormsg.error pos "integer required"

let actual_ty ty = ty

let trans_exp (venv : venv) (_ : tenv) (exp : A.exp) : expty = 
  let rec trexp exp =
    match exp with
    | A.VarExp v -> trvar v
    | A.NilExp -> {exp=(); ty=Types.NIL}
    | A.UnitExp -> {exp=(); ty=Types.UNIT}
    | A.IntExp _ -> {exp=(); ty=Types.INT}
    | A.StringExp (_, _) ->
      {exp=(); ty=Types.STRING}
    | A.OpExp {left; oper=_; right; pos} ->
      check_int (trexp left) pos;
      check_int (trexp right) pos;
      {exp=(); ty=Types.INT}
    | _ -> raise NotImplemented
  and trvar var =
    match var with
    | A.SimpleVar (id, pos) -> (
      match S.look venv id with
      | Some (E.VarEntry {ty}) ->
        {exp=(); ty=actual_ty ty}
      | None -> 
        Errormsg.error pos ("undefined variable " ^ (S.name id));
        {exp=(); ty=Types.UNIT}
      | _ -> raise NotImplemented
    )
    | A.FieldVar _ -> raise NotImplemented
    | A.SubscriptVar _ -> raise NotImplemented
  in
    trexp exp