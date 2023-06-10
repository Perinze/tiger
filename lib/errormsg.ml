let any_errors = ref false
let filename = ref ""
let line_num = ref 1
let line_pos = ref [1]

let reset () =
  any_errors := false;
  filename := "";
  line_num := 1;
  line_pos := [1];

exception Error

let error (pos : int) (msg : string) : unit =
  let rec look = function
    | (a::rest, n) ->
      if a < pos then
        print_endline (":" ^ string_of_int n ^ "." ^ string_of_int (pos-a))
      else
        look (rest, n-1)
    | ([], _) -> print_endline "0.0"
  in
    any_errors := true;
    print_endline !filename;
    look (!line_pos, !line_num);
    print_endline (":" ^ msg)



let impossible msg =
  print_endline ("Error: Compiler bug: " ^ msg);
  raise Error