open Printf

let () =
  let usage () =
    eprintf "Usage:\n  labwork1 p4 [--min 100] [--max 999]\n  labwork1 p26 [--limit 1000]\n";
    exit 1
  in
  match Array.to_list Sys.argv with
  | _ :: "p4" :: rest ->
      let minv = ref 100 and maxv = ref 999 in
      let rec parse = function
        | [] -> ()
        | "--min" :: v :: tl -> minv := int_of_string v; parse tl
        | "--max" :: v :: tl -> maxv := int_of_string v; parse tl
        | _ -> usage ()
      in
      parse rest;
      let open Lib.P04 in
      let a1 = monolithic_tail ~min:!minv ~max:!maxv in
      let a2 = monolithic_recursive ~min:!minv ~max:!maxv in
      let a3 = modular_pipeline ~min:!minv ~max:!maxv in
      let a4 = map_generation ~min:!minv ~max:!maxv in
      let a5 = loops_version ~min:!minv ~max:!maxv in
      let a6 = lazy_seq_version ~min:!minv ~max:!maxv in
      printf "Problem 4 (largest palindrome product in [%d..%d]):\n" !minv !maxv;
      printf " tail-recursive       : %d\n" a1;
      printf " non-tail recursion   : %d\n" a2;
      printf " modular pipeline     : %d\n" a3;
      printf " map-based generation : %d\n" a4;
      printf " loops (imperative)   : %d\n" a5;
      printf " lazy Seq             : %d\n" a6
  | _ :: "p26" :: rest ->
      let limit = ref 1000 in
      let rec parse = function
        | [] -> ()
        | "--limit" :: v :: tl -> limit := int_of_string v; parse tl
        | _ -> usage ()
      in
      parse rest;
      let open Lib.P26 in
      let r1 = monolithic_tail !limit in
      let r2 = monolithic_recursive !limit in
      let r3 = modular_pipeline !limit in
      let r4 = map_generation !limit in
      let r5 = loops_version !limit in
      let r6 = lazy_seq_version !limit in
      printf "Problem 26 (1/d, d < %d, longest recurring cycle):\n" !limit;
      printf " tail-recursive       : %d\n" r1;
      printf " non-tail recursion   : %d\n" r2;
      printf " modular pipeline     : %d\n" r3;
      printf " map-based generation : %d\n" r4;
      printf " loops (imperative)   : %d\n" r5;
      printf " lazy Seq             : %d\n" r6
  | _ -> usage ()
