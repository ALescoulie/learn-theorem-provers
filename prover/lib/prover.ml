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

let rec prove (g : (proof * prop) list) (p : prop) : proof option = 
  match find_proof g p with
  | Some m -> Some m
  | None ->
    match p with
    | Prod (a, b) ->
      begin match (prove g a, prove g b) with
      | (Some PA, Some PB) -> Some (Pair (PA, PB))
      | _ -> None
      end
    | _ -> None

