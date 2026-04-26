type prop = 
  | A
  | B
  | C
  | Prod of prop * prop
[@@deriving show]

type proof =
  | PA
  | PB
  | PC
  | Pair of proof * proof
  | Fst of proof
  | Snd of proof
[@@deriving show]

let rec find_proof (g : (proof * prop) list) (p : prop) : proof option =
  match g with
  | [] -> None
  | (m, p') :: _ when p = p' -> Some m
  | (_, p') :: t when p <> p' -> find_proof t p
  | _ -> None

let rec is_equal (m : proof) (a : prop) : bool =
  match (m, a) with
  | (PA, A) -> true
  | (PB, B) -> true
  | (PC, C) -> true
  | (Pair (n, p), Prod (b, c)) -> (is_equal n b) && (is_equal p c)
  | _ -> false

let is_pair (g : proof * prop) : bool =
  match g with
  | (_, Prod _) -> true
  | _ -> false

let rec is_fst_in_pair (g : proof * prop) (p : prop) : bool =
  match (g, p) with
  | (((Pair (m, _)), Prod (a, _)), p') -> is_equal m p' && a = p'
  | _ -> false

let rec is_snd_in_pair (g : proof * prop) (p : prop) : bool =
  match (g, p) with
  | (((Pair (_, m)), Prod (_, a)), p') -> is_equal m  p' && a = p'
  | _ -> false

let rec prove (g : (proof * prop) list) (p : prop) : proof option = 
  match find_proof g p with
  | Some m -> Some m
  | None ->
    match p with
    | Prod (a, b) ->
      begin match (prove g a, prove g b) with
      | (Some m, Some n) -> Some (Pair (m, n))
      | _ -> None
      end
    | _ ->
      let not_pairs = List.filter (fun x -> is_pair x |> not) g in
      match List.filter is_pair g with
      | (Pair (m, n) as pair, Prod (a, b)) :: t ->
        let fst = (Fst pair, a) in
        let snd = (Snd pair, b) in
        prove (List.append [fst; snd] not_pairs |> (fun x -> List.append x t)) p
      | (Fst (Pair _) as inner, Prod (a, b)) :: t ->
        let fst = (Fst inner, a) in
        let snd = (Snd inner, b) in
        let not_fst = List.append [fst; snd] t in
        prove (List.append not_pairs not_fst) p
      | (Snd (Pair _) as inner, Prod (a, b)) :: t ->
        let fst = (Fst inner, a) in
        let snd = (Snd inner, b) in
        let not_snd = List.append [fst; snd] t in
        prove (List.append not_pairs not_snd) p
      | _ -> None

    (*
    match p with
    | Prod (a, b) ->
      begin match (prove g a, prove g b) with
      | (Some m, Some n) -> Some (Pair (m, n))
      | _ -> None
      end
    | p' -> 
      begin match List.filter (fun x -> is_fst_in_pair x p') g with
      | (Pair (m, n), Prod (a, _)) :: t when (is_equal m p') && (a = p') ->
        Some (Fst (Pair (m, n)))
      | _ ->
        begin match List.filter (fun x-> is_snd_in_pair x p') g with
        | (Pair (m, n), Prod (_, b)) :: t when (is_equal n p') && (b = p') ->
          Some (Snd (Pair (m, n)))
        | begin match List.filter  -> None
        end
      end
      *)


