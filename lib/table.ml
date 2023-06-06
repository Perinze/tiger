module IntMapTable (KEY : sig
    type key
    val getInt : key -> int
  end
) = struct
  type key = KEY.key
  module IntMap = Map.Make(
    struct
      type t = int
      let compare = compare
    end
  )
  type 'a table = 'a IntMap.t
  let empty = IntMap.empty
  let enter (t, k, a) = IntMap.add (KEY.getInt k) a t
  let look (t, k) = IntMap.find_opt (KEY.getInt k) t
end