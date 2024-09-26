type t
let exit_with_msg msg =
  print_endline msg;
  exit 1

let rec add_constraints constraints puzzle =
        match puzzle with
        | (_, _, _, 0)::tail -> (add_constraints constraints tail) 
        | (_, row, col, num)::tail -> (add_constraints constraints tail) ^ (Printf.sprintf "(assert (= (cell %d %d) %d))" row col num) 
        | [] -> constraints

let rec evals index n =
        if index = (n * n) then "" else
        ((Printf.sprintf "(eval (cell %d %d))" (index / n) (index mod n)) ^ (evals (index + 1) n))

let rec build_solution index k n z3 =
        if index = (n * n) then [] else
        let num = (int_of_string (Z3.raw_read_line z3)) in ((k, (index / n), (index mod n), num)::(build_solution (index + 1) k n z3))

let rec uniqueness original_puzzle solved_puzzle =
        match original_puzzle, solved_puzzle with
        | ((_, row_o, col_o, 0)::tail_o), ((_, _, _, num)::tail_s) -> ((uniqueness tail_o tail_s) ^
                                                                      (Printf.sprintf "(not (= (cell %d %d) %d))" row_o col_o num))
        | ((_, _, _, _)::tail_o), ((_, _, _, _)::tail_s) -> (uniqueness tail_o tail_s) 
        | ((_, _, _, _)::_), [] -> ""
        | [] , ((_, _, _, _)::_) ->""
        | [], [] -> "" 

let () =
  let in_chan = Scanf.Scanning.stdin in
  let p = try
      Puzzle.from_channel in_chan
    with Puzzle.ParseError msg -> exit_with_msg (Printf.sprintf "parse error: %s" msg)
  in
  let k = match p with
        | (k, _, _, _)::_ -> k
        | [] -> 0 in
  let n = k * k in
  let constraints = Printf.sprintf 
"
(declare-fun cell (Int Int) Int)

(define-fun in_range ((i Int)) Bool
  (and (>= i 0) (< i %d)))

(assert
  (forall ((row Int) (col Int))
    (and (>= (cell row col) 1) (<= (cell row col) %d))))

(assert
  (forall ((row Int) (i Int) (j Int)) (=>
    (and
      (not (= i j))
      (in_range row)
      (in_range i)
      (in_range j))
    (not (= (cell row i) (cell row j))))))

(assert
  (forall ((col Int) (i Int) (j Int)) (=>
    (and
      (not (= i j))
      (in_range col)
      (in_range i)
      (in_range j))
    (not (= (cell i col) (cell j col))))))

(assert
  (forall ((row1 Int) (col1 Int) (row2 Int) (col2 Int)) (=>
      (and
        (in_range row1) (in_range col1) (in_range row2) (in_range col2)
        (or (not (= row1 row2)) (not (= col1 col2)))
        (= (div row1 %d) (div row2 %d))
        (= (div col1 %d) (div col2 %d)))
      (not (= (cell row1 col1)
              (cell row2 col2))))))
" n n k k k k in
  let z3 = Z3.init () in
  Z3.raw_send z3 ((add_constraints constraints p) ^ "(check-sat)");
  let sat = ((Z3.raw_read_line z3) = "sat") in
  if sat then Z3.raw_send z3 (evals 0 n) else print_endline "unsat";
  if sat then let solved_puzzle = (build_solution 0 k n z3) in print_string (Puzzle.show solved_puzzle);
  if sat then let u_constraints = ("(assert (or " ^ (uniqueness p solved_puzzle) ^ ")) (check-sat)") in (Z3.raw_send z3 u_constraints);
  if sat then let u_sat = ((Z3.raw_read_line z3) = "sat") in
  if sat then if u_sat then print_endline "found more than one solution!" else print_endline "puzzle solution is unique";
  if sat then if u_sat then Z3.raw_send z3 (evals 0 n);
  if sat then if u_sat then let second_solution = (build_solution 0 k n z3) in print_string (Puzzle.show second_solution);
  Z3.close z3
