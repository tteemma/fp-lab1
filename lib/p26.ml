module U = Utils

(* 1) Монолитная хвостовая рекурсия *)
let monolithic_tail limit =
  let rec search d best_d best_len =
    if d = 1 then best_d
    else
      let len = U.recurring_cycle_len d in
      if len > best_len then search (d - 1) d len
      else search (d - 1) best_d best_len
  in
  search (limit - 1) 0 0

(* 2) Монолитная (нехвостовая) рекурсия *)
let monolithic_recursive limit =
  let rec best_to d =
    if d <= 1 then (0, 0)
    else
      let (bd, bl) = best_to (d - 1) in
      let l = U.recurring_cycle_len d in
      if l > bl then (d, l) else (bd, bl)
  in
  fst (best_to (limit - 1))

(* 3) Модульный конвейер *)
let modular_pipeline limit =
  List.init (limit - 2) (fun i -> i + 2)
  |> List.map (fun d -> (d, U.recurring_cycle_len d))
  |> List.fold_left
       (fun (bd, bl) (d, l) -> if l > bl then (d, l) else (bd, bl))
       (0, 0)
  |> fst

(* 4) Генерация через map *)
let map_generation limit =
  List.init (limit - 2) (fun i -> i + 2)
  |> List.map (fun d -> (d, U.recurring_cycle_len d))
  |> List.fold_left
       (fun (bd, bl) (d, l) -> if l > bl then (d, l) else (bd, bl))
       (0, 0)
  |> fst

(* 5) Вариант с циклами *)
let loops_version limit =
  let best_d = ref 0 in
  let best_l = ref 0 in
  for d = 2 to limit - 1 do
    let l = U.recurring_cycle_len d in
    if l > !best_l then (best_l := l; best_d := d)
  done;
  !best_d

(* 6) Ленивые последовательности (Seq) *)
let lazy_seq_version limit =
  let open Seq in
  let numbers = unfold (fun d -> if d >= limit then None else Some (d, d + 1)) 2 in
  let step (bd, bl) d =
    let l = U.recurring_cycle_len d in
    if l > bl then (d, l) else (bd, bl)
  in
  numbers |> fold_left step (0, 0) |> fst
