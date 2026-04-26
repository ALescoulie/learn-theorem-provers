open Prover

let test_prove (g : (proof * prop) list) (p : prop) (expected : proof option) =
  let result = prove g p in

  let ok = result = expected in

  Printf.printf "Test prove:\n";
  let show_pair (m, p) = "(" ^ show_proof m ^ ", " ^ show_prop p ^ ")" in
  let input =
    List.map show_pair g
    |> String.concat "; "
  in

  let show_proof_option = function
    | Some m -> "Some " ^ show_proof m
    | None -> "None"
  in

  Printf.printf "  inputs: [%s], %s\n" input (show_prop p);
  Printf.printf "  result: %s\n" (show_proof_option result);
  Printf.printf "  expected: %s\n" (show_proof_option expected);
  Printf.printf "  %s\n\n" (if ok then "PASS" else "FAIL")

let () =
  test_prove [] A None;
  test_prove [(PA, A); (PB, B)] (Prod (A, B)) (Some (Pair (PA, PB)))

