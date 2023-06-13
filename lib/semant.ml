module A = Absyn
module E = Env
module S = Symbol
module T = Types
exception NotImplemented

type venv = E.enventry S.table
type tenv = T.ty S.table

module Translate = struct
  type exp = unit
end

type expty = {exp : Translate.exp; ty : T.ty}
type env = {tenv : tenv; venv : venv}

let trans_prog (_ : A.exp) : unit =
  ()

let check_int {exp=_; ty=ty} pos =
  match ty with
  | T.INT -> ()
  | _ -> Errormsg.error pos "integer required"

let actual_ty ty = ty

let rec trans_exp (venv : venv) (tenv : tenv) (exp : A.exp) : expty = 
  let rec trexp exp =
    match exp with
    | A.VarExp v -> trvar v
    | A.NilExp -> {exp=(); ty=T.NIL}
    | A.UnitExp -> {exp=(); ty=T.UNIT}
    | A.IntExp _ -> {exp=(); ty=T.INT}
    | A.StringExp (_, _) ->
      {exp=(); ty=T.STRING}
    | A.OpExp {left; oper=_; right; pos} ->
      check_int (trexp left) pos;
      check_int (trexp right) pos;
      {exp=(); ty=T.INT}
    | _ -> raise NotImplemented
  and trvar var =
    match var with
    | A.SimpleVar (id, pos) -> (
      match S.look venv id with
      | Some (E.VarEntry {ty=ty}) ->
        {exp=(); ty=actual_ty ty}
      | None -> 
        Errormsg.error pos ("undefined variable " ^ (S.name id));
        {exp=(); ty=T.UNIT}
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
  | A.TypeDec [{name=id;ty=ty}] ->
    {venv=venv;
     tenv=S.enter id (trans_ty tenv ty) tenv}
  | _ -> raise NotImplemented

and trans_ty (tenv : tenv) (ty : A.ty) =
  let look id pos =
    match S.look tenv id with
    | Some t -> t
    | None ->
      Errormsg.error pos ("unbound type identifier: " ^ (S.name id));
      UNIT in
  match ty with
  | NameTy (id, pos) -> look id pos
  | RecordTy fields ->
    let f ({name=id;escape=_;typ=typ;pos=pos} : A.field) =
      (id, look typ pos) in
    RECORD (List.map f fields, ref ())
  | ArrayTy (id, pos) -> ARRAY (look id pos, ref ())