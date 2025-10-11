let is_palindrome n =
  let s = string_of_int n in
  let len = String.length s in
  let rec aux i j =
    if i >= j then true
    else if s.[i] <> s.[j] then false
    else aux (i + 1) (j - 1)
  in
  aux 0 (len - 1)

(* Длина периода десятичной дроби для 1/d *)
let recurring_cycle_len d =
  (* pos.(r) = первая позиция появления остатка r; -1 если не встречался *)
  let pos = Array.make d (-1) in
  let rec loop remainder index =
    if remainder = 0 then 0
    else
      let r = remainder mod d in
      if pos.(r) <> -1 then index - pos.(r)
      else (
        pos.(r) <- index;
        loop ((r * 10) mod d) (index + 1))
  in
  loop (1 mod d) 0
