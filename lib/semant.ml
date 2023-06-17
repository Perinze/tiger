module A = Absyn
module E = Env
module S = Symbol
module T = Types
exception NotImplemented of string

type venv = E.enventry S.table
type tenv = T.ty S.table

module Translate = struct
  type exp = unit
end

type expty = {exp : Translate.exp; ty : T.ty}
type env = {tenv : tenv; venv : venv}

let tlook (tenv : tenv) (sym : S.symbol) pos =
  match S.look tenv sym with
  | Some a -> a
  | None -> Errormsg.error pos ("Unbound type name: " ^ S.name sym); UNIT

(*
let vlook (venv : venv) (sym : S.symbol) pos =
  match S.look venv sym with
  | Some a -> a
  | None -> Errormsg.error pos ("Unbound variable name: " ^ S.name sym); DummyEntry
*)

let check_int {exp=_; ty=ty} pos =
  match ty with
  | T.INT -> ()
  | _ -> Errormsg.error pos "integer required"

let actual_ty ty = ty

let rec trans_prog (exp : A.exp) : unit =
  let _ = trans_exp E.base_venv E.base_tenv exp in
  ()

and trans_exp (venv : venv) (tenv : tenv) (exp : A.exp) : expty = 
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
    | _ -> raise (NotImplemented "trexp")
  and trvar var =
    match var with
    | A.SimpleVar (id, pos) -> (
      match S.look venv id with
      | Some (E.VarEntry {ty=ty}) ->
        {exp=(); ty=actual_ty ty}
      | None -> 
        Errormsg.error pos ("undefined variable " ^ (S.name id));
        {exp=(); ty=T.UNIT}
      | _ -> raise (NotImplemented "simple var for function")
    )
    | _ -> raise (NotImplemented "trvar")
  in
    match exp with
    | A.LetExp {decs=decs;body=e;pos=_} ->
      let f {venv=v;tenv=t} dec =
        trans_dec v t dec in
      let {venv=venv';tenv=tenv'} =
        List.fold_left f {venv=venv;tenv=tenv} decs in
      trans_exp venv' tenv' e
    
    | A.SeqExp exps ->
      let f _ (exp, _) = trans_exp venv tenv exp in
      List.fold_left f {exp=();ty=T.UNIT} exps 

    | _ -> trexp exp


and trans_dec (venv : venv) (tenv : tenv) (dec : A.dec) : env =
  match dec with
  | A.VarDec {name=id;escape=_;typ=typ;init=init;pos=_} ->
    let {exp=_;ty=ty} = trans_exp venv tenv init in
    (match typ with
    | None ->
      {tenv=tenv; venv=S.enter id (E.VarEntry {ty=ty}) venv}
    | _ -> raise (NotImplemented "var dec with type annotation"))

  | A.TypeDec decs ->
    let f tenv ({name=id;ty=ty;pos=_} : A.typedec) =
      S.enter id (trans_ty tenv ty) tenv in
    {venv=venv; tenv=List.fold_left f tenv decs}

  | A.FunctionDec decs -> (* very tricky functions *)

    (* transform an (parameter : A.field) to (varsym, T.ty) *)
    let trparam (tenv : tenv) ({name=id;typ=tid;pos=pos;_} : A.field) : S.symbol * T.ty =
      (id, tlook tenv tid pos) in

    (* reduce venv and (param : (varsym, T.ty)) to venv' *)
    let bindparam venv (id, ty) =
      S.enter id (E.VarEntry {ty=ty}) venv in

    (* reduce venv and fundec to venv' *)
    let trfundec venv ({name=id;params=params;body=body;result=result;_} : A.fundec) : venv =
      let params' : (S.symbol * T.ty) list = List.map (trparam tenv) params in
      (* venv + formal_params *)
      let venv' : venv = List.fold_left bindparam venv params' in
      (* traverse body to infer result type as ty *)
      let {ty=inferrty;_} : expty = trans_exp venv' tenv body in
      (* compare inferred type with annotated type *)
      let checked_rty =
        match result with
        | None -> inferrty (* no annotated type*)
        | Some (rsym, p) ->
          if (tlook tenv rsym p) = inferrty then
            inferrty
          else
            (Errormsg.error p ("mismatched result type: " ^ (S.name rsym)); T.UNIT)
      in
        (* finally, return venv + fundec *)
        S.enter id (E.FunEntry {formals=List.map snd params';result=checked_rty}) venv
    in
      (* reduce all fundecs and return venv + [fundec] *)
      {tenv=tenv;venv=List.fold_left trfundec venv decs}


and trans_ty (tenv : tenv) (ty : A.ty) =
  match ty with
  | NameTy (id, pos) -> tlook tenv id pos
  | RecordTy fields ->
    let f ({name=id;escape=_;typ=typ;pos=pos} : A.field) =
      (id, tlook tenv typ pos) in
    RECORD (List.map f fields, ref ())
  | ArrayTy (id, pos) -> ARRAY (tlook tenv id pos, ref ())