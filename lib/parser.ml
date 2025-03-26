type formula =
  | Var of string
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Implies of formula * formula
  | Equiv of formula * formula

type input = char list
type 'a parser_result = Failed | Parsed of 'a * input
type 'a parser = input -> 'a parser_result

let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun input ->
  match p input with Failed -> Failed | Parsed (v, rest) -> (f v) rest

let ( *> ) (p : 'a parser) (q : 'b parser) : 'b parser = p >>= fun _ -> q

let ( <* ) (p : 'a parser) (q : 'b parser) : 'a parser =
  p >>= fun v ->
  q >>= fun _ -> fun rest -> Parsed (v, rest)

let ( <|> ) (p : 'a parser) (q : 'a parser) : 'a parser =
 fun input ->
  match p input with Failed -> q input | Parsed (v, rest) -> Parsed (v, rest)

let char (c : char) : char parser = function
  | x :: xs when x = c -> Parsed (c, xs)
  | _ -> Failed

let string (s : string) : input parser =
  let rec helper s =
    match s with
    | [] -> fun input -> Parsed ([], input)
    | x :: xs -> char x *> helper xs
  in
  helper (List.of_seq (String.to_seq s))

let variable : formula parser =
  let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let letter : char parser = function
    | c :: rest when is_letter c -> Parsed (c, rest)
    | _ -> Failed
  in
  let rec helper acc s =
    match letter s with
    | Failed -> if acc = "" then Failed else Parsed (Var acc, s)
    | Parsed (c, rest) -> helper (acc ^ String.make 1 c) rest
  in
  helper ""

let rec atom input =
  (variable
  <|> (char '~' *> atom >>= fun f -> fun rest -> Parsed (Not f, rest))
  <|> (char '(' *> formula <* char ')'))
    input

and conj input =
  ( atom >>= fun left ->
    (string "/\\" *> atom >>= fun right -> fun rest -> Parsed (And (left, right), rest))
    <|> (string "\\/" *> atom >>= fun right -> fun rest -> Parsed (Or (left, right), rest))
    <|> fun rest -> Parsed (left, rest) )
    input

and implies input =
  ( conj >>= fun left ->
    (string "->" *> formula >>= fun right -> fun rest -> Parsed (Implies (left, right), rest))
    <|> fun rest -> Parsed (left, rest) )
    input

and formula input =
  ( implies >>= fun left ->
    (string "<->" *> formula >>= fun right -> fun rest -> Parsed (Equiv (left, right), rest))
    <|> fun rest -> Parsed (left, rest) )
    input

let parse_formula s = formula (List.of_seq (String.to_seq s))

