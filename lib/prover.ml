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