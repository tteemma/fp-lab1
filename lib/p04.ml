module U = Utils

let largest_palindrome_product ~min ~max =
  let best = ref 0 in
  for i = max downto min do
    for j = i downto min do
      let p = i * j in
      if p > !best && U.is_palindrome p then best := p
    done
  done;
  !best

(* 1) Монолитная хвостовая рекурсия *)
let monolithic_tail ~min ~max =
  let rec inner i j best =
    if i < min then best
    else if j < min then inner (i - 1) (i - 1) best
    else
      let p = i * j in
      let best' = if p > best && U.is_palindrome p then p else best in
      inner i (j - 1) best'
  in
  inner max max 0

(* 2) Монолитная (нехвостовая) рекурсия *)
let monolithic_recursive ~min ~max =
  let rec check_row i j =
    if j < min then 0
    else
      let p = i * j in
      let best_here = if U.is_palindrome p then p else 0 in
      Stdlib.max best_here (check_row i (j - 1))
  in
  let rec rows i =
    if i < min then 0 else Stdlib.max (check_row i i) (rows (i - 1))
  in
  rows max

(* 3) Модульный конвейер: generate -> filter -> reduce *)
let modular_pipeline ~min ~max =
  let open List in
  let gen_pairs min max =
    init (max - min + 1) (fun k -> k + min)
    |> concat_map (fun i ->
         init (max - i + 1) (fun k -> k + i)
         |> map (fun j -> (i, j)))
  in
  gen_pairs min max
  |> map (fun (i, j) -> i * j)
  |> filter U.is_palindrome
  |> fold_left Stdlib.max 0



(* 4) Генерация через map (map -> fold) *)
let map_generation ~min ~max =
  let pairs =
    let acc = ref [] in
    for i = min to max do
      for j = i to max do
        acc := (i, j) :: !acc;
      done
    done;
    !acc
  in
  pairs
  |> List.map (fun (i, j) -> i * j)
  |> List.map (fun p -> (p, U.is_palindrome p))
  |> List.fold_left
       (fun b (p, ok) -> if ok && p > b then p else b)
       0


(* 5) Вариант с циклами *)
let loops_version ~min ~max = largest_palindrome_product ~min ~max

(* 6) Ленивые последовательности (Seq) *)
let lazy_seq_version ~min ~max =
  let open Seq in
  let range a b = unfold (fun i -> if i > b then None else Some (i, i + 1)) a in
  let pairs =
    range min max
    |> flat_map (fun i -> map (fun j -> (i, j)) (range i max))
  in
  pairs
  |> map (fun (i, j) -> i * j)
  |> filter U.is_palindrome
  |> fold_left Stdlib.max 0
