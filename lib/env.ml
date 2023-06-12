type ty = Types.ty

module M = Map.Make (
  struct
    type t = int
    let compare = compare
  end
)

type env = ty M.t

let empty = M.empty

let find env symbol =
  M.find_opt (snd symbol) env

let update env symbol ty =
  M.update (snd symbol) (fun _ -> Some ty) env