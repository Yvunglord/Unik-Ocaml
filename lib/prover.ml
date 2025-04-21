open Parser

type proof_state = {
  goal : formula;
  context: formula list;
  remaining_goals: formula list;
}

type tactic = 
| EquivIntro
| Intros
| NotIntros
| Axiom
| Apply
| AndElim
| Contradiction
| Qed

let rec formula_equal f1 f2 =
  match (f1, f2) with
  | Var s1, Var s2 -> s1 = s2
  | Not f1', Not f2' -> formula_equal f1' f2'
  | And (a1, b1), And (a2, b2)
  | Or (a1, b1), Or (a2, b2)
  | Implies (a1, b1), Implies (a2, b2)
  | Equiv (a1, b1), Equiv (a2, b2) ->
      formula_equal a1 a2 && formula_equal b1 b2
  | _ -> false

  let apply_tactic state tactic =
    match (state.goal, tactic) with
    | Equiv (f1, f2), EquivIntro ->
        let new_state =
          {
            state with
            remaining_goals = Implies (f2, f1) :: state.remaining_goals;
            goal = Implies (f1, f2);
          }
        in
        Ok new_state
    | Implies (f1, f2), Intros ->
        let new_state = { state with context = f1 :: state.context; goal = f2 } in
        Ok new_state
    | Not f, NotIntros ->
        Ok { state with goal = Var "False"; context = f :: state.context }
    | f, NotIntros ->
        Ok { state with goal = Var "False"; context = Not f :: state.context }
    | _, AndElim ->
        let new_context =
          List.fold_left
            (fun acc f ->
              match f with And (a, b) -> a :: b :: acc | _ -> f :: acc)
            [] state.context
        in
        Ok { state with context = new_context }
    | _, Apply ->
        let new_context =
          List.fold_left
            (fun acc f ->
              match f with
              | Implies (a, b) when List.mem a state.context -> b :: acc
              | _ -> f :: acc)
            [] state.context
        in
        Ok { state with context = new_context }
    | goal, Axiom -> (
        let has_axiom =
          List.exists (fun f -> formula_equal f goal) state.context
        in
        match (state.remaining_goals, has_axiom) with
        | _, false -> Error "goal is not in context"
        | [], true -> Ok { state with goal = Var "proved" }
        | new_goal :: tl, true ->
            Ok { state with goal = new_goal; remaining_goals = tl })
    | _, Contradiction -> (
        let has_contradiction =
          List.exists
            (fun f ->
              List.exists (fun g -> formula_equal f (Not g)) state.context)
            state.context
        in
        match (state.remaining_goals, has_contradiction) with
        | _, false -> Error "no contradiction in context"
        | [], true -> Ok { state with goal = Var "proved" }
        | new_goal :: tl, true ->
            Ok { state with goal = new_goal; remaining_goals = tl })
    | goal, Qed ->
        if formula_equal goal (Var "proved") && state.remaining_goals = [] then
          Ok state
        else Error "proof incomplete"
    | _ -> Error "tactic not applicable to current goal"
  
  let parse_tactic s =
    match String.lowercase_ascii s with
    | "equivintro" | "eq" -> EquivIntro
    | "intros" | "in" -> Intros
    | "notintros" | "no" -> NotIntros
    | "axiom" | "ax" -> Axiom
    | "apply" | "ap" -> Apply
    | "andelim" | "an" -> AndElim
    | "contradiction" | "co" -> Contradiction
    | "qed" | "qe" -> Qed
    | _ -> failwith ("Unknown tactic: " ^ s)
  
  let init_proof_state s =
    match parse_formula s with
    | Parsed (f, []) -> { goal = f; context = []; remaining_goals = [] }
    | Parsed (_, _) -> failwith "Extra characters after formula"
    | Failed -> failwith "Failed to parse formula"
  
  let rec string_of_formula = function
    | Var v -> v
    | Not f -> "~" ^ string_of_formula f
    | And (f1, f2) ->
        Printf.sprintf "(%s /\\ %s)" (string_of_formula f1) (string_of_formula f2)
    | Or (f1, f2) ->
        Printf.sprintf "(%s \\/ %s)" (string_of_formula f1) (string_of_formula f2)
    | Implies (f1, f2) ->
        Printf.sprintf "(%s -> %s)" (string_of_formula f1) (string_of_formula f2)
    | Equiv (f1, f2) ->
        Printf.sprintf "(%s <-> %s)" (string_of_formula f1) (string_of_formula f2)
  
  let print_state state =
    Printf.printf "Current goal: %s\n" (string_of_formula state.goal);
    if state.context <> [] then
      Printf.printf "Context:\n  %s\n"
        (String.concat "\n  " (List.map string_of_formula state.context));
    if state.remaining_goals <> [] then
      Printf.printf "Remaining goals: %d\n" (List.length state.remaining_goals);
    print_newline ()
  
  let rec proof_loop state input_channel =
    print_state state;
    Printf.printf "Enter tactic (or 'help' for list): %!";
    try
      let input = input_line input_channel |> String.trim in
      if input = "help" then (
        Printf.printf "\nAvailable tactics:\n";
        Printf.printf "  equivintro/eq - Introduce equivalence\n";
        Printf.printf "  intros/in     - Introduce implication\n";
        Printf.printf "  notintros/no  - Introduce negation\n";
        Printf.printf "  axiom/ax      - Use axiom from context\n";
        Printf.printf "  apply/ap      - Apply implication from context\n";
        Printf.printf "  andelim/an    - Eliminate conjunction\n";
        Printf.printf "  contradiction/co - Find contradiction\n";
        Printf.printf "  qed/qe        - Complete proof\n\n";
        proof_loop state input_channel)
      else if input = "" then proof_loop state input_channel
      else
        let tactic = parse_tactic input in
        match apply_tactic state tactic with
        | Ok new_state ->
            if formula_equal new_state.goal (Var "proved") && new_state.remaining_goals = [] then
              Printf.printf "\nProof completed successfully!\n"
            else
              proof_loop new_state input_channel
        | Error msg ->
            Printf.printf "Error: %s\n" msg;
            proof_loop state input_channel
    with End_of_file ->
      Printf.printf "\nEnd of input reached. Exiting.\n";
      exit 0
  
  let interactive_mode () =
    Printf.printf "Interactive Proof Assistant\n";
    Printf.printf "Enter initial goal: %!";
    let goal = read_line () in
    proof_loop (init_proof_state goal) stdin
  
  let test_mode test_input =
    let channel = open_in test_input in
    try
      let goal = input_line channel in
      proof_loop (init_proof_state goal) channel
    with e ->
      close_in channel;
      raise e
  
  let () =
    if Array.length Sys.argv > 1 then
      test_mode Sys.argv.(1)
    else
      interactive_mode ()