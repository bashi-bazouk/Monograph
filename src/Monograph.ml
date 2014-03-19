open List
open Str
open Unix

open Util
open OCamlDep

(* Util *)

(** read_relative d returns the filepath in directory d relative to d. *)
let read_relative (d: string): string list =
  Array.to_list (Array.map (fun f -> d ^ "/" ^ f) (Sys.readdir d))

(* End Util *)

let root =
  fst (Array.fold_left 
	 (fun (root, prev) arg -> if prev = "-d" then (arg, arg) else (root, arg))
	 (".", "")
	 Sys.argv)

let directories =
  let rec directories_tr (acc: string list) = function
    | [] -> acc
    | hd::tl ->
      if Sys.is_directory hd then
	directories_tr (hd::acc) ((read_relative hd)@tl)
      else
	directories_tr acc tl in
  directories_tr ["."] (Array.to_list (Sys.readdir root))

let files =
  let valid f =
    let (_, basename, extension) = split_path f in
    (String.length basename > 0) && basename.[0] <> '.' && extension = ".ml" in
  filter valid (root::(concat (map read_relative directories)))

let dependencies =
  let includes = map ((^) "-I ") directories in
  ocamldep ~args:(includes@files@["-native"])

let targets =
  let nonzero = (<>) 0
  and reachability f1 f2 =
      let t1 = basename f1
      and t2 = basename f2 in
      let d1 = dependencies [t1]
      and d2 = dependencies [t2] in
      if mem t1 d2 then -1 else if mem t2 d1 then 1 else 0 in
  let connectivity f =
    fold_left (fun acc f' -> acc + (abs (reachability f f'))) 0 files in
  sort 
    (fun f1 f2 ->
      let reachability = reachability f1 f2 in
      if nonzero reachability then 
	reachability
      else
	let connectivities = (connectivity f1) - (connectivity f2) in
	if nonzero connectivities then
	  connectivities
	else
	  (stat f1).st_size - (stat f2).st_size)
    files
       
let () =
  let print_module f =
    let module_name = String.capitalize (basename f) in
    try
      let inp = open_in f in
      try
	print_endline ("module "^module_name^" = struct");
	while true do
	  print_endline ("  " ^ (input_line inp))
	done
      with End_of_file -> 
	print_endline "end\n";
	close_in inp;
    with _ -> () in
  List.iter print_module targets;
