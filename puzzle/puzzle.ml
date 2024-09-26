type t = (int * int * int * int) list

exception ParseError of string

let from_channel s =
  let get_int () =
    try
      Scanf.bscanf s " %d" (fun n -> n)
    with
      Scanf.Scan_failure msg ->
      raise (ParseError (Printf.sprintf "failed to read integer: %s" msg))
    | End_of_file ->
      raise (ParseError "Unexpected end of file")
  in
  let k = get_int () in
  if k <= 0 then raise (ParseError (Printf.sprintf "k must be > 0, but got %d" k));
  if k > 100 then raise (ParseError (Printf.sprintf "k must be <= 100, got %d" k));
  (* Call get_int k**4 times and store the numbers in the puzzle *)
  let total = k * k * k * k in
  let n = k * k in
  let rec create_puzzle counter =
          if counter = 0 then [] else
          let get = get_int () in
          let index = total - counter in ((k, (index / n), (index mod n), get)::(create_puzzle (counter - 1))) in
  create_puzzle total
;;

let show t =
        let rec concat_next s =
                match s with
                | [] -> ""
                | (k, _, col, num)::tail -> if ((col mod (k * k)) = ((k * k) - 1)) then ((Printf.sprintf "%d \n" num) ^ (concat_next tail))
                else ((Printf.sprintf "%d " num) ^ (concat_next tail)) in
        match t with
        | [] -> ""
        | (k, _, _, _)::_ -> ((Printf.sprintf "%d\n" k) ^ concat_next t)

let%test_module _ =
  (module struct
     let p = from_channel (Scanf.Scanning.from_string
                             "2\n1 2 3 4\n3 4 1 2\n2 1 4 3\n4 3 2 1\n")
     let%expect_test _ =
       print_endline (show p);
       [%expect{|
         2
         1 2 3 4
         3 4 1 2
         2 1 4 3
         4 3 2 1
       |}]
   end)
