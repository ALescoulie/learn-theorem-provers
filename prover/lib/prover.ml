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

let rec unfold_pair (g : proof * prop) : ((proof * prop) list) =
  match g with
  | (m, Prod (a, b)) as g -> g :: (List.append (unfold_pair (Fst m, a)) (unfold_pair (Snd m, b)))
  | g' -> [g']

let rec prove (g : (proof * prop) list) (p : prop) : proof option = 
  match find_proof g p with
  | Some m -> Some m
  | None ->
    let g' = List.flatten (List.map unfold_pair g) in
    match p with
    | Prod (a, b) ->
      begin match (find_proof g' a, find_proof g' b) with
      | (Some m, Some n) -> Some (Pair (m, n))
      | _ -> None
      end
    | _ -> 
      match find_proof g' p with
      | Some m -> Some m
      | None -> None

