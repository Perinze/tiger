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
  | None -> Errormsg.error pos ("error : unknown type " ^ S.name sym); UNIT

let vlook (venv : venv) (sym : S.symbol) pos =
  match S.look venv sym with
  | Some a -> a
  | None -> Errormsg.error pos ("Unbound variable name: " ^ S.name sym); DummyEntry

let check_int {exp=_; ty=ty} pos =
  match ty with
  | T.INT -> ()
  | _ -> Errormsg.error pos "error : integer required"

let actual_ty (ty : T.ty) : T.ty =
  match ty with
  (* name type : return its content *)
  | NAME (_, {contents=Some t}) ->
    t

  | NAME (_, {contents=None}) ->
    Errormsg.impossible "an empty name type"; UNIT

  (* compound type : recursively traverse *)
  (*
  | RECORD (sts, unique) ->
    RECORD (List.map (fun (s, t) -> (s, actual_ty t)) sts, unique)

  | ARRAY (ty, unique) ->
    ARRAY ((actual_ty ty), unique)
  *)

  (* other type : return itself *)
  | _ -> ty

let rec trans_prog (exp : A.exp) : unit =
  let _ = trans_exp E.base_venv E.base_tenv exp in
  ()

and trans_exp (venv : venv) (tenv : tenv) (exp : A.exp) : expty = 
  let rec trexp = function
  | A.DummyExp -> {exp=(); ty=UNIT}
  | A.VarExp v -> trvar v
  | A.NilExp -> {exp=(); ty=T.NIL}
  | A.UnitExp -> {exp=(); ty=T.UNIT}
  | A.IntExp _ -> {exp=(); ty=T.INT}
  | A.StringExp (_, _) ->
    {exp=(); ty=T.STRING}

  | A.CallExp {func;args;pos} ->
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
    else (
      let formal_len = List.length formal_tys in
      let arg_len = List.length arg_tys in
      if formal_len = arg_len then
        Errormsg.error pos "error : formals and actuals have different types"
      else if formal_len > arg_len then
        Errormsg.error pos "error : formals are more than actuals"
      else 
        Errormsg.error pos "error : formals are fewer than actuals";
      {exp=();ty=UNIT}
    )

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
    let trarg (sym, exp, pos) : S.symbol * T.ty * Lexing.position =
      let {ty;_} = trexp exp in
      (sym, ty, pos)
    in

    (* args'' is with type instead of exp *)
    let args'' = List.map trarg args' in

    (* local type of correspondence between arg and field *)
    let module Local = struct
      type corres =
      | Match
      | OnlyArg of S.symbol * T.ty * A.pos
      | MissArg of S.symbol * T.ty
      end
    in

    (* pair args with fields depending on their symbols *)
    let rec pair (args : (S.symbol * T.ty * A.pos) list) (fields : (S.symbol * T.ty) list) =
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
    if varty != expty then (
      if expty = NIL then
        match varty with
        | RECORD _ -> ()
        | _ -> Errormsg.error pos "error : type mismatch"
      else
        Errormsg.error pos "error : type mismatch"
    );
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
    let aty = actual_ty (tlook tenv typ pos) in
    let ety =
      match aty with
      | ARRAY (t, _) -> actual_ty t
      | _ ->
        Errormsg.error pos ("Type " ^ (S.name typ) ^ " is not an array type.");
        UNIT in
    let {ty=init_ty;_} = trexp init in
    if ety != init_ty then
      Errormsg.error pos "error : initializing exp and array type differ";
    {exp=();ty=aty} 

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
        | _ ->
          Errormsg.error pos "error : variable not record";
          [] in
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
        Errormsg.error pos "error : variable not array";
        {exp=(); ty=UNIT}
    )
  in
    trexp exp


and trans_dec (venv : venv) (tenv : tenv) (dec : A.dec) : env =
  match dec with
  | A.VarDec {name=id;typ;init;pos;_} ->
    let {exp=_;ty=ty} = trans_exp venv tenv init in
    (match typ with
    | None ->
      if ty = NIL then
        Errormsg.error pos
        "initializing nil expressions not constrained by record type";
      {tenv=tenv; venv=S.enter id (E.VarEntry {ty=ty}) venv}
    | Some (typ', p) ->
      let dty = tlook tenv typ' p in
      if dty != ty then
        if ty = NIL then
          match dty with
          | RECORD _ -> ()
          | _ ->
            Errormsg.error pos
            "initializing nil expressions not constrained by record type"
        else
          Errormsg.error pos "error : type constraint and init value differ";
      {tenv=tenv; venv=S.enter id (E.VarEntry {ty=dty}) venv})

  (* TODO check if types have same name : test38 *)
  (* TODO check mutually recursive types that do not pass through record : test16 *)
  | A.TypeDec decs ->

    (* reduce tenv and typedec to tenv' without traverse it's defination *)
    let bindtypedec tenv ({name=id;_} : A.typedec) : tenv =
      S.enter id (T.NAME (id, ref None)) tenv in

    (* tenv + all type dec, for recursion *)
    let tenv' = List.fold_left bindtypedec tenv decs in

    (* traverse typedec A.ty and update tenv's name entries (ref) *)
    let trdec tenv ({name;ty;pos} : A.typedec) : unit =
      let r : T.ty option ref =
        match tlook tenv name pos with
        | T.NAME (s, r) ->
          if s != name then
            Errormsg.impossible "different symbols in key-value pair";
          r
        | _ ->
          Errormsg.impossible "expect a name type";
          ref None
      in
      r := Some (trans_ty tenv ty)
    in
        
    (* assign trans_ty's returns to refs in entries of tenv *)
    List.iter (trdec tenv') decs;

    (* map dec with actual_ty of it in tenv' *)
    let mapentry tenv ({name;pos;_} : A.typedec) : tenv =
      S.enter name (tlook tenv' name pos |> actual_ty) tenv in

    {venv=venv; tenv=List.fold_left mapentry tenv decs}

  (* TODO check if functions have same name : test39 *)
  | A.FunctionDec decs -> (* very tricky functions *)

    (* reduce venv and fundec to venv' without traversing it's body *)
    let bindfundec venv ({name=id;params;result;_} : A.fundec) : venv =
      S.enter id (E.FunEntry {
        formals =
          List.map
            (fun (field : A.field) -> tlook tenv field.typ field.pos)
            params;
        result =
          match result with
          | None -> UNIT
          | Some (s, p) -> tlook tenv s p
      }) venv in

    (* venv + all function dec, for recursion *)
    let venv' =
      List.fold_left bindfundec venv decs in

    (* transform an (parameter : A.field) to (varsym, T.ty) *)
    let trparam (tenv : tenv) ({name=id;typ=tid;pos=pos;_} : A.field) : S.symbol * T.ty =
      (id, tlook tenv tid pos) in

    (* reduce venv and (param : (varsym, T.ty)) to venv' *)
    let bindparam venv (id, ty) =
      S.enter id (E.VarEntry {ty=ty}) venv in
    
    (* traverse fundec body *)
    let trfundec venv ({params;body;result;pos;_} : A.fundec) : unit =
      let params' : (S.symbol * T.ty) list = List.map (trparam tenv) params in
      (* venv + formal_params *)
      let venv' : venv = List.fold_left bindparam venv params' in
      (* traverse body to infer result type as ty *)
      let {ty=inferrty;_} : expty = trans_exp venv' tenv body in
      (* compare inferred type with annotated type *)
      match result with
      | None ->
        if inferrty != UNIT then
          Errormsg.error pos "error : procedure returns value";
      | Some (rsym, p) ->
        if (tlook tenv rsym p) != inferrty then
          Errormsg.error p ("mismatched result type: " ^ (S.name rsym))
    in

    (* traverse all fundecs' body for type-checking *)
    List.iter (trfundec venv') decs;
    (* return venv with all function dec *)
    {tenv=tenv;venv=venv'}


and trans_ty (tenv : tenv) (ty : A.ty) : T.ty =
  match ty with
  | NameTy (id, pos) -> tlook tenv id pos
  | RecordTy fields ->
    let f ({name=id;typ=typ;pos=pos;_} : A.field) =
      (id, tlook tenv typ pos) in
    RECORD (List.map f fields, ref ())
  | ArrayTy (id, pos) -> ARRAY (tlook tenv id pos, ref ())