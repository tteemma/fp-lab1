open Alcotest

module P04 = Lib.P04
module P26 = Lib.P26
(* при желании можно тестировать и утилиты: module U = Lib.Utils *)

(* ---------- Problem 4 ---------- *)

let p4_solvers =
  [
    ("tail", fun ~min ~max -> P04.monolithic_tail ~min ~max);
    ("recursive", fun ~min ~max -> P04.monolithic_recursive ~min ~max);
    ("modular", fun ~min ~max -> P04.modular_pipeline ~min ~max);
    ("map", fun ~min ~max -> P04.map_generation ~min ~max);
    ("loops", fun ~min ~max -> P04.loops_version ~min ~max);
    ("seq", fun ~min ~max -> P04.lazy_seq_version ~min ~max);
  ]

let test_p4_small_range () =
  (* руками проверяем маленький диапазон — самый большой палиндром в [10..99] *)
  let expected = 9009 (* 91 * 99 *) in
  List.iter
    (fun (label, f) -> check int label expected (f ~min:10 ~max:99))
    p4_solvers

let test_p4_full_range () =
  let expected = 906_609 (* 913 * 993 *) in
  List.iter
    (fun (label, f) -> check int label expected (f ~min:100 ~max:999))
    p4_solvers

(* ---------- Problem 26 ---------- *)

let p26_solvers =
  [
    ("tail", P26.monolithic_tail);
    ("recursive", P26.monolithic_recursive);
    ("modular", P26.modular_pipeline);
    ("map", P26.map_generation);
    ("loops", P26.loops_version);
    ("seq", P26.lazy_seq_version);
  ]

let test_p26_small () =
  (* быстрые проверки на малых значениях *)
  check int "limit<10 -> best d=7" 7 (P26.monolithic_tail 10);
  check int "limit<8  -> best d=7" 7 (P26.monolithic_tail 8)

let test_p26_all_solvers () =
  let limit = 1000 in
  let expected = 983 in
  List.iter
    (fun (label, f) -> check int label expected (f limit))
    p26_solvers

let () =
  run "labwork1"
    [
      ( "Euler 4",
        [
          test_case "palindrome small [10..99]" `Quick test_p4_small_range;
          test_case "palindrome full  [100..999]" `Slow test_p4_full_range;
        ] );
      ( "Euler 26",
        [
          test_case "quick sanity small" `Quick test_p26_small;
          test_case "all solvers d<1000" `Slow test_p26_all_solvers;
        ] );
    ]
