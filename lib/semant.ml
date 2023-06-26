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

let vlook (venv : venv) (sym : S.symbol) pos =
  match S.look venv sym with
  | Some a -> a
  | None -> Errormsg.error pos ("Unbound variable name: " ^ S.name sym); DummyEntry

let check_int {exp=_; ty=ty} pos =
  match ty with
  | T.INT -> ()
  | _ -> Errormsg.error pos "integer required"

let actual_ty ty = ty

let rec trans_prog (exp : A.exp) : unit =
  let _ = trans_exp E.base_venv E.base_tenv exp in
  ()

and trans_exp (venv : venv) (tenv : tenv) (exp : A.exp) : expty = 
  let rec trexp = function
  | A.VarExp v -> trvar v
  | A.NilExp -> {exp=(); ty=T.NIL}
  | A.UnitExp -> {exp=(); ty=T.UNIT}
  | A.IntExp _ -> {exp=(); ty=T.INT}
  | A.StringExp (_, _) ->
    {exp=(); ty=T.STRING}

  | A.CallExp {func=func;args=args;pos=pos} ->
    let func' = vlook venv func pos in
    let func'' =
      match func' with
      | FunEntry _ ->
        func'
      | _ -> Errormsg.error pos ("Not a function: " ^ (S.name func));
        DummyEntry in
    let (formal_tys, result_ty) =
      match func'' with
      | FunEntry {formals=formals;result=rt} -> (formals, rt)
      | _ -> ([], T.UNIT) in
    let trformal (arg : A.exp) =
      (trans_exp venv tenv arg).ty in
    let arg_tys =
      List.map trformal args in
    if formal_tys = arg_tys then
      {exp=();ty=result_ty}
    else
      {exp=();ty=UNIT}

  | A.OpExp {left;right;pos;_} ->
    check_int (trexp left) pos;
    check_int (trexp right) pos;
    {exp=(); ty=T.INT}

  | A.RecordExp {fields=args;typ;pos;_} ->

    (* look up typ and check if it's a record type *)
    (* if yes, extract its field list *)
    let fields' : (S.symbol * T.ty) list =
      match tlook tenv typ pos with
      | RECORD (f, _) -> f
      | _ ->
        Errormsg.error pos ("type " ^ S.name typ ^ " is not a record");
        []
    in
    (* sort field list *)
    let fields'' = List.sort (fun (a, _) (b, _) -> (snd a) - (snd b)) fields' in

    (* sort arg list *)
    let args' = List.sort (fun (a, _, _) (b, _, _) -> (snd a) - (snd b)) args in

    (* traverse arg's exp and replace it with its type *)
    let trarg (sym, exp, pos) : S.symbol * T.ty * int =
      let {ty;_} = trexp exp in
      (sym, ty, pos)
    in

    (* args'' is with type instead of exp *)
    let args'' = List.map trarg args' in

    (* local type of correspondence between arg and field *)
    let module Local = struct
      type corres =
      | Match
      | OnlyArg of S.symbol * T.ty * int
      | MissArg of S.symbol * T.ty
      end
    in

    (* pair args with fields depending on their symbols *)
    let rec pair (args : (S.symbol * T.ty * int) list) (fields : (S.symbol * T.ty) list) =
      match args with
      | [] -> (
        match fields with
        | [] -> []
        | (fsym, fty) :: frest -> (Local.MissArg (fsym, fty)) :: (pair [] frest)
      )
      | (asym, aty, apos) :: arest -> (
        match fields with
        | [] -> (Local.OnlyArg (asym, aty, apos)) :: (pair arest [])
        | (fsym, fty) :: frest ->
          if asym = fsym then
            Local.Match :: pair arest frest
          else if (snd asym) < (snd fsym) then
            Local.OnlyArg (asym, aty, apos) :: (pair arest ((fsym, fty) :: frest))
          else
            Local.MissArg (fsym, fty) :: (pair ((asym, aty, apos) :: arest) frest)
      )
    in

    (* pairing result *)
    let argfield = pair args'' fields'' in

    (* check and print error in need *)
    let check (c : Local.corres) =
      match c with
      | Local.Match -> ()
      | Local.OnlyArg (s, _, p) -> Errormsg.error p ("There is no field " ^ (S.name s) ^ " within type " ^ (S.name typ))
      | Local.MissArg (s, _) -> Errormsg.error pos ("Field " ^ (S.name s) ^ " is undefined")
    in

    (* iter, and return record expty *)
    List.iter check argfield;
    {exp=(); ty=tlook tenv typ pos}
  
  | A.SeqExp exps ->
    let f _ (exp, _) = trans_exp venv tenv exp in
    List.fold_left f {exp=();ty=T.UNIT} exps 

  | A.AssignExp {var;exp;pos} ->
    let {ty=varty;_} = trvar var in
    let {ty=expty;_} = trexp exp in
    if varty != expty then
      Errormsg.error pos "Type mismatch";
    {exp=(); ty=UNIT}

  | A.IfExp {test;then';else';pos} ->
    let {ty=test_ty;_} = trexp test in
    if test_ty != INT then
      Errormsg.error pos "Test expression must has type int.";
    let {ty=then_ty;_} = trexp then' in
    let ty =
      match else' with
      | None ->
        if then_ty != UNIT then
          Errormsg.error pos "error : if-then returns non unit";
        T.UNIT
      | Some else'' -> (
        let {ty=else_ty;_} = trexp else'' in
          if else_ty = then_ty then
            then_ty
          else (
            Errormsg.error pos "error : types of then - else differ";
            UNIT 
          )
      ) in
    {exp=(); ty=ty}

  | A.WhileExp {test;body;pos} ->
    let {ty=test_ty;_} = trexp test in
    if test_ty != INT then
      Errormsg.error pos "Test expression must has type int.";
    let {ty=body_ty;_} = trexp body in
    if body_ty != UNIT then
      Errormsg.error pos "error : body of while not unit";
    {exp=(); ty=UNIT}

  | A.ForExp {var;lo;hi;body;pos;_} ->
    let {ty=lo_ty;_} = trexp lo in
    let {ty=hi_ty;_} = trexp hi in
    if lo_ty != INT || hi_ty != INT then
      Errormsg.error pos "For-loop range must has type int.";
    let venv' = S.enter var (E.VarEntry {ty=INT}) venv in
    let _ = trans_exp venv' tenv body in
    {exp=();ty=UNIT}

  | A.BreakExp _ -> {exp=();ty=UNIT}

  | A.LetExp {decs=decs;body=e;pos=_} ->
    let f {venv=v;tenv=t} dec =
      trans_dec v t dec in
    let {venv=venv';tenv=tenv'} =
      List.fold_left f {venv=venv;tenv=tenv} decs in
    trans_exp venv' tenv' e

  | A.ArrayExp {typ;size;init;pos} ->
    let {ty=size_ty;_} = trexp size in
    if size_ty != INT then
      Errormsg.error pos "Array size must has type int.";
    let aty = tlook tenv typ pos in
    let ety =
      match aty with
      | ARRAY (t, _) -> t
      | _ ->
        Errormsg.error pos ("Type " ^ (S.name typ) ^ " is not an array type.");
        UNIT in
    let {ty=init_ty;_} = trexp init in
    if ety != init_ty then
      Errormsg.error pos "Init expression has unexpected type";
    {exp=();ty=aty} 

  | _ -> raise (NotImplemented "trexp")

  and trvar var =
    match var with
    | A.SimpleVar (id, pos) -> (
      match S.look venv id with
      | Some (E.VarEntry {ty=ty}) ->
        {exp=(); ty=actual_ty ty}
      | None -> 
        Errormsg.error pos ("error: undeclared variable " ^ (S.name id));
        {exp=(); ty=T.UNIT}
      | _ -> raise (NotImplemented "simple var for function")
    )
    | A.FieldVar (v, id, pos) ->
      let {ty;_} = trvar v in
      let fields =
        match ty with
        | RECORD (sym_ty_list, _) -> sym_ty_list
        | _ -> [] in
      let pred (sym, _) = sym = id in
      let fieldty =
        match List.find_opt pred fields with
        | Some (_, ty) -> ty
        | None ->
          Errormsg.error pos ("error : field " ^ S.name id ^ " not in record type");
          UNIT in
      {exp=(); ty=fieldty}
    | A.SubscriptVar (v, exp, pos) -> (
      let {ty=index_ty;_} = trexp exp in
      if index_ty != INT then
        Errormsg.error pos "Array must be indexed with int.";
      let {ty;_} = trvar v in
      match ty with
      | ARRAY (ty, _) -> {exp=(); ty=ty}
      | _ ->
        Errormsg.error pos "Not an array.";
        {exp=(); ty=UNIT}
    )
  in
    trexp exp


and trans_dec (venv : venv) (tenv : tenv) (dec : A.dec) : env =
  match dec with
  | A.VarDec {name=id;escape=_;typ;init;pos} ->
    let {exp=_;ty=ty} = trans_exp venv tenv init in
    (match typ with
    | None ->
      {tenv=tenv; venv=S.enter id (E.VarEntry {ty=ty}) venv}
    | Some (typ', p) ->
      let dty = tlook tenv typ' p in
      if dty != ty then
        Errormsg.error pos ("Init exp doesn't have type " ^ (S.name typ'));
      {tenv=tenv; venv=S.enter id (E.VarEntry {ty=ty}) venv})

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