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
type env = {tenv : tenv; venv : venv}

let trans_prog (_ : A.exp) : unit =
  ()

let check_int {exp=_; ty=ty} pos =
  match ty with
  | Types.INT -> ()
  | _ -> Errormsg.error pos "integer required"

let actual_ty ty = ty

let rec trans_exp (venv : venv) (tenv : tenv) (exp : A.exp) : expty = 
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
      | Some (E.VarEntry {ty=ty}) ->
        {exp=(); ty=actual_ty ty}
      | None -> 
        Errormsg.error pos ("undefined variable " ^ (S.name id));
        {exp=(); ty=Types.UNIT}
      | _ -> raise NotImplemented
    )
    | _ -> raise NotImplemented
  in
    match exp with
    | A.LetExp {decs=decs;body=e;pos=_} ->
      let f {venv=v;tenv=t} dec =
        trans_dec v t dec in
      let {venv=venv';tenv=tenv'} =
        List.fold_left f {venv=venv;tenv=tenv} decs in
      trans_exp venv' tenv' e
    | _ -> trexp exp

and trans_dec (venv : venv) (tenv : tenv) (dec : A.dec) : env =
  match dec with
  | A.VarDec {name=id;escape=_;typ=typ;init=init;pos=_} ->
    let {exp=_;ty=ty} = trans_exp venv tenv init in
    (match typ with
    | None ->
      {tenv=tenv; venv=S.enter id (E.VarEntry {ty=ty}) venv}
    | _ -> raise NotImplemented)
  | _ -> raise NotImplemented