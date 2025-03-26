module type SET = sig
  type elem
  type t

  val empty : t
  val add : elem -> t -> t
  val delete : elem -> t -> t
  val member : elem -> t -> bool
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module RedBlackTree (Element : ORDERED) : sig
  type color = Red | Black
  type elem = Element.t
  type t = private Empty | Node of color * t * elem * t

  include SET with type elem := Element.t and type t := t
end = struct
  type color = Red | Black
  type elem = Element.t
  type t = Empty | Node of color * t * elem * t

  let empty = Empty

  let balance = function
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
        Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | color, a, x, b -> Node (color, a, x, b)

  let rec add x = function
    | Empty -> Node (Red, Empty, x, Empty)
    | Node (color, a, y, b) as node ->
        match Element.compare x y with
        | 0 -> node
        | n when n < 0 -> balance (color, add x a, y, b)
        | _ -> balance (color, a, y, add x b)

  let add x t =
    match add x t with
    | Node (_, a, y, b) -> Node (Black, a, y, b)
    | Empty -> failwith "unreachable"

  let rec member x = function
    | Empty -> false
    | Node (_, a, y, b) ->
        match Element.compare x y with
        | 0 -> true
        | n when n < 0 -> member x a
        | _ -> member x b

  let rec delete_min = function
    | Empty -> failwith "empty"
    | Node (_, Empty, y, _) -> y, Empty
    | Node (color, a, y, b) ->
        let min, a' = delete_min a in
        min, Node (color, a', y, b)

  let balance_delete = function
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
        Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | color, a, x, b -> Node (color, a, x, b)

  let rec delete x = function
    | Empty -> Empty
    | Node (color, a, y, b) ->
        match Element.compare x y with
        | 0 ->
            if b = Empty then a
            else
              let min, b' = delete_min b in
              balance_delete (color, a, min, b')
        | n when n < 0 -> balance_delete (color, delete x a, y, b)
        | _ -> balance_delete (color, a, y, delete x b)

  let delete x t =
    match delete x t with
    | Empty -> Empty
    | Node (_, a, y, b) -> Node (Black, a, y, b)
end

module IntSet = RedBlackTree(struct
  type t = int
  let compare = compare
end)

let () =
  let set = IntSet.empty in
  let set = IntSet.(set |> add 10 |> add 20 |> add 30) in
  Printf.printf "Member 20: %b\n" (IntSet.member 20 set);
  Printf.printf "Member 15: %b\n" (IntSet.member 15 set);
  let set = IntSet.(set |> delete 20) in
  Printf.printf "Member 20 after delete: %b\n" (IntSet.member 20 set)
