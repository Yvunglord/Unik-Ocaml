(* Factorial *)
let rec fact n = if n < 2 then 1 else n * fact (n-1)

let reverse lst =
    let rec helper acc lst =
      match lst with [] -> acc | h :: tail -> helper (h :: acc) tail
    in
    helper [] lst

let filter pred lst =
    let rec helper acc lst =
        match lst with
        | [] -> List.rev acc
        | h :: tail -> 
            if pred h then helper (h :: acc) tail
            else helper acc tail
        in 
        helper [] lst

let option fn x y z =
    match (x, y, z) with
    | (Some x_val, Some y_val, Some z_val) -> Some (fn x_val y_val z_val)
    | _ -> None

let map f lst =
    let rec helper acc lst =
        match lst with
        | [] -> List.rev acc  
        | h :: tail -> helper (f h :: acc) tail 
    in
    helper [] lst

let map_2 f list1 list2 =
    let rec helper acc list1 list2 =
        match (list1, list2) with
        | ([], _) | (_, []) -> List.rev acc  
        | (h1 :: tail1, h2 :: tail2) -> helper (f h1 h2 :: acc) tail1 tail2  
    in
    helper [] list1 list2 

let find pred lst =
    let rec helper lst =
        match lst with
        | [] -> raise Not_found  
        | h :: tail ->
            if pred h then h  
            else helper tail
    in
    helper lst

let find_opt pred lst =
    let rec helper lst =
        match lst with
        | [] -> None  
        | h :: tail ->
            if pred h then Some h  
            else helper tail  
    in
    helper lst

let cartesian list1 list2 =
    let rec helper acc list1 list2 =
      match list1 with
      | [] -> List.rev acc
      | h1 :: tail1 ->
          let pairs = List.map (fun h2 -> (h1, h2)) list2 in
          helper (pairs @ acc) tail1 list2
    in
    helper [] list1 list2

let cartesianN lists =
let rec helper acc lists =
    match lists with
    | [] -> List.map List.rev acc
    | h :: tail ->
        let new_acc = List.fold_left (fun acc' x -> List.fold_left (fun acc'' y -> (y :: x) :: acc'') acc' h) [] acc in
        helper new_acc tail
    in
    helper [[]] lists

let concat lists =
let rec helper acc lists =
    match lists with
    | [] -> List.rev acc
    | h :: tail -> helper (List.rev_append h acc) tail
in
helper [] lists

let concat_map lst ~f =
let rec helper acc lst =
    match lst with
    | [] -> List.rev acc
    | h :: tail -> helper (List.rev_append (f h) acc) tail
in
helper [] lst

let rec fold_left f acc = function
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let fold_right f lst init =
  fold_left (fun g x -> fun acc -> g (f x acc)) (fun x -> x) lst init

let add x y z = x + y + z

let%expect_test "optionOneNone" =
    let result = option add (Some 1) None (Some 3) in
    match result with
    | Some value -> Printf.printf "Result: %d\n" value
    | None -> Printf.printf "Result: None\n";
    [%expect {|Result: None|}]

let%expect_test "optionAllNone" =
    let result = option add None None None in
    match result with
    | Some value -> Printf.printf "Result: %d\n" value
    | None -> Printf.printf "Result: None\n";
    [%expect {|Result: None|}]

let%expect_test "test1" = print_int (fact 5);
[%expect {|120|}]
let%expect_test "test2" = print_int (fact 10);
[%expect {|3628800|}]

let%expect_test "reversedList" =
  let reversed = reverse [1; 2; 3; 4; 5] in
  List.iter (Printf.printf "%d ") reversed;
  [%expect {|5 4 3 2 1 |}]

let%expect_test "reversedEmptyList" =
  let reversed = reverse [] in
  List.iter (Printf.printf "%d ") reversed;
  [%expect {||}]

let%expect_test "reversedSingleElementList" =
  let reversed = reverse [42] in
  List.iter (Printf.printf "%d ") reversed;
  [%expect {|42 |}]

  let%expect_test "filterEvenNumbers" =
  let filtered = filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6] in
  List.iter (Printf.printf "%d ") filtered;
  [%expect {|2 4 6 |}]

let%expect_test "filterEmptyList" =
  let filtered = filter (fun x -> x > 0) [] in
  List.iter (Printf.printf "%d ") filtered;
  [%expect {||}]

let%expect_test "filterAllElements" =
  let filtered = filter (fun x -> x > 0) [1; 2; 3; 4; 5] in
  List.iter (Printf.printf "%d ") filtered;
  [%expect {|1 2 3 4 5 |}]

let%expect_test "filterNoElements" =
  let filtered = filter (fun x -> x < 0) [1; 2; 3; 4; 5] in
  List.iter (Printf.printf "%d ") filtered;
  [%expect {||}]

let%expect_test "filterSingleElement" =
  let filtered = filter (fun x -> x = 42) [42] in
  List.iter (Printf.printf "%d ") filtered;
  [%expect {|42 |}]

let%expect_test "mapExample" =
  let result = map (fun x -> x * 2) [1; 2; 3; 4] in
  List.iter (Printf.printf "%d ") result;
  [%expect {|2 4 6 8 |}]

let%expect_test "mapEmptyList" =
  let result = map (fun x -> x * 2) [] in
  List.iter (Printf.printf "%d ") result;
  [%expect {||}]

let%expect_test "map_2Example" =
  let result = map_2 (fun x y -> x + y) [1; 2; 3] [4; 5; 6] in
  List.iter (Printf.printf "%d ") result;
  [%expect {|5 7 9 |}]

let%expect_test "map_2DifferentLengths" =
  let result = map_2 (fun x y -> x * y) [1; 2; 3] [4; 5] in
  List.iter (Printf.printf "%d ") result;
  [%expect {|4 10 |}]

let%expect_test "map_2EmptyLists" =
  let result = map_2 (fun x y -> x + y) [] [1; 2; 3] in
  List.iter (Printf.printf "%d ") result;
  [%expect {||}]

let%expect_test "findExample" =
  let result = find (fun x -> x > 2) [1; 2; 3; 4] in
  Printf.printf "Found: %d\n" result;
  [%expect {|Found: 3|}]

let%expect_test "findNotFound" =
  try
    let _ = find (fun x -> x > 10) [1; 2; 3; 4] in
    Printf.printf "Found\n"
  with Not_found ->
    Printf.printf "Not found\n";
  [%expect {|Not found|}]

let%expect_test "find_optExample" =
  let result = find_opt (fun x -> x > 2) [1; 2; 3; 4] in
  (match result with
   | Some value -> Printf.printf "Found: %d\n" value
   | None -> Printf.printf "Not found\n");
  [%expect {|Found: 3|}]

let%expect_test "find_optNotFound" =
  let result = find_opt (fun x -> x > 10) [1; 2; 3; 4] in
  (match result with
   | Some value -> Printf.printf "Found: %d\n" value
   | None -> Printf.printf "Not found\n");
  [%expect {|Not found|}]

let%expect_test "cartesianExample" =
  let result = cartesian [1; 2] ['a'; 'b'] in
  List.iter (fun (x, y) -> Printf.printf "(%d, %c) " x y) result;
  [%expect {|(1, b) (1, a) (2, b) (2, a) |}]

let%expect_test "concatExample" =
  let result = concat [[1; 2]; [3; 4]; [5; 6]] in
  List.iter (Printf.printf "%d ") result;
  [%expect {|1 2 3 4 5 6 |}]

let%expect_test "concatEmptyList" =
  let result = concat [] in
  List.iter (Printf.printf "%d ") result;
  [%expect {||}]

(* Тесты для concat_map *)
let%expect_test "concat_mapExample" =
  let result = concat_map [1; 2; 3] ~f:(fun x -> [x; x * 10]) in
  List.iter (Printf.printf "%d ") result;
  [%expect {|1 10 2 20 3 30 |}]

let%expect_test "concat_mapEmptyList" =
  let result = concat_map [] ~f:(fun x -> [x; x * 10]) in
  List.iter (Printf.printf "%d ") result;
  [%expect {||}]

  let%expect_test "fold_left sum" =
  let result = fold_left (fun a b -> a + b) 0 [1; 2; 3; 4] in
  Printf.printf "Sum: %d\n" result;
  [%expect {| Sum: 10 |}]

let%expect_test "fold_right concat" =
  let result = fold_right (fun x acc -> x :: acc) [1; 2; 3] [] in
  let str = List.map string_of_int result |> String.concat "; " in
  Printf.printf "List: [%s]\n" str;
  [%expect {| List: [1; 2; 3] |}]

let%expect_test "fold_right sum" =
  let result = fold_right (fun x acc -> x + acc) [1; 2; 3; 4] 0 in
  Printf.printf "Sum: %d\n" result;
  [%expect {| Sum: 10 |}]