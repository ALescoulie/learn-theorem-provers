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
    | p' -> 
      begin match List.filter (fun x -> is_fst_in_pair x p') g with
      | (Pair (m, n), Prod (a, _)) :: t when (is_equal m p') && (a = p') ->
        Some (Fst (Pair (m, n)))
      | _ ->
        begin match List.filter (fun x-> is_snd_in_pair x p') g with
        | (Pair (m, n), Prod (_, b)) :: t when (is_equal n p') && (b = p') ->
          Some (Snd (Pair (m, n)))
        | _ -> None
        end
      end
      


