# Лабораторная работа №1

**Выполнил:** Мироненко Артем Дмитриевич  
**Группа:** P3331  
**Языки:** OCaml, Go

---

## Задача 4

**Найти наибольший палиндром, являющийся произведением двух трёхзначных чисел.**

---

### Реализации (OCaml)

#### `monolithic_tail`

Монолитная реализация с **хвостовой рекурсией**.  
На каждом шаге проверяются произведения чисел `i * j`, при этом результат предыдущих шагов передаётся в аккумулятор `best`.  
Рекурсия продолжается, пока не перебраны все комбинации в диапазоне `[min..max]`.  
Благодаря хвостовой оптимизации алгоритм не расходует стек и эффективно по памяти.

#### `monolithic_recursive`

Классическая **рекурсивная** реализация без хвостовой оптимизации.  
Для каждого числа `i` рекурсивно вычисляется максимальный палиндром в строке (`check_row`), затем вызывается сама для следующего числа.  
Подходит для наглядного понимания рекурсивного обхода, но не оптимальна при больших диапазонах.

#### `modular_pipeline`

Модульная версия, использующая функциональный конвейер (**генерация → фильтрация → свёртка**).  
Генерируются все пары чисел, фильтруются только палиндромные произведения, и из них берётся максимум.  
Показывает преимущества **декларативного стиля** и композиции функций (`map`, `filter`, `fold_left`).

#### `map_generation`

Генерация всех пар чисел через **map**, создание списка их произведений, затем фильтрация по признаку палиндрома.  
Сочетает простоту и выразительность, демонстрируя принципы **работы с функциями высшего порядка**.

#### `loops_version`

Императивная версия через **циклы for**.  
Использует изменяемую переменную `best`.  
Этот вариант ближе к традиционным языкам (например, Go, C), но менее идиоматичен для функционального стиля.

#### `lazy_seq_version`

Использует **ленивые последовательности** (`Seq`).  
Создаётся поток всех возможных пар чисел, далее с помощью `map` и `filter` из него извлекается максимум палиндромных произведений.  
Преимущество — не требуется хранить весь список в памяти, данные генерируются «на лету».

---

### Реализации (OCaml, `lib/p04.ml`)

```ocaml
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
  let gen_pairs =
    let acc = ref [] in
    for i = min to max do
      for j = i to max do
        acc := (i, j) :: !acc
      done
    done;
    !acc
  in
  gen_pairs
  |> map (fun (i, j) -> i * j)
  |> filter U.is_palindrome
  |> fold_left Stdlib.max 0

(* 4) Генерация через map (map -> fold) *)
let map_generation ~min ~max =
  let pairs =
    let acc = ref [] in
    for i = min to max do
      for j = i to max do
        acc := (i, j) :: !acc
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

```

---

### Реализация (Go)

Императивный вариант, аналогичный `loops_version` из OCaml.  
Перебираются все пары чисел, проверяется палиндромность произведения, результат сохраняется в `best`.  
Go используется для демонстрации **традиционного итеративного подхода** и сравнения парадигм.

---

#### Go-версия (`go/p04.go`)

```go
package lib

import "strconv"

func isPalindrome(n int) bool {
	s := strconv.Itoa(n)
	for i := 0; i < len(s)/2; i++ {
		if s[i] != s[len(s)-1-i] {
			return false
		}
	}
	return true
}

func LargestPalindromeProduct(min, max int) int {
	best := 0
	for i := max; i >= min; i-- {
		for j := i; j >= min; j-- {
			p := i * j
			if p > best && isPalindrome(p) {
				best = p
			}
		}
	}
	return best
}
```

---

## Задача 26

**Найти число d < 1000, для которого период десятичной дроби 1/d имеет максимальную длину.**

---

### Реализации (OCaml)

#### `monolithic_tail`

Хвосторекурсивная версия, которая перебирает все значения d от limit–1 до 1, вычисляя длину периода (`Utils.recurring_cycle_len`).  
Аккумуляторы `best_d` и `best_len` хранят текущий максимум.  
Этот вариант оптимален по памяти.

#### `monolithic_recursive`

Рекурсивная версия без хвостовой оптимизации.  
Функция вызывает себя для каждого следующего делителя и сравнивает длину периодов.  
Подходит для демонстрации чистой рекурсии.

#### `modular_pipeline`

Конвейерное решение — создаётся список всех делителей, для каждого вычисляется длина периода, а затем берётся элемент с максимальной длиной.  
Использует `List.init`, `map` и `fold_left`, подчёркивая **модульный и декларативный стиль**.

#### `map_generation`

Похожая реализация, но делается акцент на **сопоставлении и фильтрации через map/fold**.  
Пример использования функций как данных.

#### `loops_version`

Императивная реализация с циклами `for`.  
Прямая и наглядная, аналогична алгоритму на Go.  
Использует изменяемые ссылки `ref`.

#### `lazy_seq_version`

Использует **ленивые последовательности** (`Seq`).  
Позволяет перебирать делители без предварительной генерации полного списка.  
Идеально подходит для больших диапазонов благодаря «ленивому» вычислению.

---

### Реализации (OCaml, `lib/p26.ml`)

```ocaml
module U = Utils

let monolithic_tail limit =
  let rec search d best_d best_len =
    if d = 1 then best_d
    else
      let len = U.recurring_cycle_len d in
      if len > best_len then search (d - 1) d len
      else search (d - 1) best_d best_len
  in
  search (limit - 1) 0 0

let monolithic_recursive limit =
  let rec best_to d =
    if d <= 1 then (0, 0)
    else
      let (bd, bl) = best_to (d - 1) in
      let l = U.recurring_cycle_len d in
      if l > bl then (d, l) else (bd, bl)
  in
  fst (best_to (limit - 1))

let modular_pipeline limit =
  List.init (limit - 2) (fun i -> i + 2)
  |> List.map (fun d -> (d, U.recurring_cycle_len d))
  |> List.fold_left (fun (bd, bl) (d, l) -> if l > bl then (d, l) else (bd, bl)) (0, 0)
  |> fst

let map_generation limit =
  List.init (limit - 2) (fun i -> i + 2)
  |> List.map (fun d -> (d, U.recurring_cycle_len d))
  |> List.fold_left (fun (bd, bl) (d, l) -> if l > bl then (d, l) else (bd, bl)) (0, 0)
  |> fst

let loops_version limit =
  let best_d = ref 0 in
  let best_l = ref 0 in
  for d = 2 to limit - 1 do
    let l = U.recurring_cycle_len d in
    if l > !best_l then (best_l := l; best_d := d)
  done;
  !best_d

let lazy_seq_version limit =
  let open Seq in
  let numbers = unfold (fun d -> if d >= limit then None else Some (d, d + 1)) 2 in
  let step (bd, bl) d =
    let l = U.recurring_cycle_len d in
    if l > bl then (d, l) else (bd, bl)
  in
  numbers |> fold_left step (0, 0) |> fst
```

---

### Реализация (Go)

Императивный подход, аналогичный OCaml-варианту с циклами.  
Функция `RecurringCycleLen` вычисляет длину периода через массив остатков, а `LongestCycle` ищет максимальную длину.  
Классический пример использования **мутабельных переменных и циклов**.

---

#### Go-версия (`go/p26.go`)

```go
package lib

func RecurringCycleLen(d int) int {
	pos := make([]int, d)
	for i := 0; i < d; i++ {
		pos[i] = -1
	}
	remainder := 1 % d
	index := 0
	for remainder != 0 {
		r := remainder % d
		if pos[r] != -1 {
			return index - pos[r]
		}
		pos[r] = index
		remainder = (r * 10) % d
		index++
	}
	return 0
}

func LongestCycle(limit int) int {
	bestD, bestL := 0, 0
	for d := 2; d < limit; d++ {
		l := RecurringCycleLen(d)
		if l > bestL {
			bestL, bestD = l, d
		}
	}
	return bestD
}
```

---

# Общие выводы

Работа демонстрирует богатство выразительных средств OCaml и важность выбора правильного подхода в зависимости от свойств задачи. Функциональный подход показывает свои преимущества в обработке рекурсивных задач и композиции функций: хвостовая рекурсия обеспечивает эффективность без переполнения стека, неизменяемость данных упрощает отладку, а модульные функции легко комбинируются в читаемые пайплайны. Ленивые вычисления с Seq эффективно работают с большими данными, экономя память, в то время как императивные циклы предлагают прямолинейное и быстрое решение для простых операций.

| Подход              | Преимущества                          | Недостатки                                 | Подходит для                         |
| ------------------- | ------------------------------------- | ------------------------------------------ | ------------------------------------ |
| Монолитная рекурсия | Простая и наглядная                   | Возможное переполнение стека               | Малые диапазоны                      |
| Хвостовая рекурсия  | Эффективна по памяти, без роста стека | Сложнее читается при вложенных вычислениях | Большие рекурсивные задачи           |
| Модульный конвейер  | Композиция функций, декларативность   | Дополнительные списки в памяти             | Читаемые и функциональные решения    |
| Map/Fold            | Чистый функциональный стиль           | Создание промежуточных структур            | Трансформации данных                 |
| Императивные циклы  | Простота, высокая производительность  | Мутации, побочные эффекты                  | Алгоритмы с линейной структурой      |
| Ленивые Seq         | Ленивые вычисления, экономия памяти   | Сложность отладки и трассировки            | Большие/потоковые данные             |
| Go (итеративно)     | Предсказуемость, скорость             | Меньшая выразительность                    | Прикладные и низкоуровневые сценарии |

---

## 📈 Результаты

Обе задачи Project Euler (№4 и №26) были корректно решены всеми способами,  
что подтверждено автоматическими тестами и CI-проверками на GitHub Actions.

| Задача                          | Результат  | Язык       | Методы                     |
| ------------------------------- | ---------- | ---------- | -------------------------- |
| №4 — Largest Palindrome Product | **906609** | OCaml / Go | Tail Recursion, Loops, Seq |
| №26 — Reciprocal Cycles         | **983**    | OCaml / Go | Map/Fold, Seq, Loops       |

---

**ИТМО, 2025**
