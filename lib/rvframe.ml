type access =
| InReg of Temp.temp
| InFrame of int

type frame = {
  name : Temp.label;
  formals : access list;
  locals : int ref;
}

let new_frame (name : Temp.label) (escapes : bool list) : frame =
  let i = ref 0 in
  let f escape : access =
    i := !i + 1;
    if escape then
      InFrame (!i * 4) (* offset by 1 because stack *)
    else
      InReg (Temp.newtemp ())
  in
  {name=name;formals=List.rev (List.map f escapes);locals=ref 0}

let name (frame : frame) : Temp.label =
  frame.name

let formals (frame : frame) : access list =
  frame.formals

let alloc_local ({locals;_} : frame) (escape : bool) : access =
  match escape with
  | false -> InReg (Temp.newtemp ())
  | true ->
      locals := !locals + 1;
      InFrame (!locals * -4)