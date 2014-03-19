open String
open Str

open Util
open TransitiveClosure

let get_line (inp: in_channel): string option =
  try
    let l = ref (input_line inp) in
    let len () =  length !l in
    let more () =
      len() = 0 || !l.[len() - 1] = '\\' in
    while more() do
      if len() = 0 then
	l := input_line inp
      else
	l := (string_before !l (len() - 1)) ^ (string_after (input_line inp) 4)
    done;
    Some !l
  with End_of_file ->
    None


let read_dep (inp: in_channel): (string * string list) option =
  let delim1 = regexp ": "
  and delim2 = regexp " " in
  match get_line inp with
    | None -> None
    | Some s ->
      match bounded_split delim1 s 2 with
	| [file] ->
	  Some (basename file, [])
	| [file; deps] ->
	  Some (basename file, List.map basename (split delim2 deps))
	| _ ->
	  failwith "unreachable"

let ocamldep ~args:(args: string list): string list -> string list =
  let args = concat " " args
  and tbl = Hashtbl.create 32 in
  let add_dep = function
    | None -> false
    | Some (k,v) ->
      List.iter (Hashtbl.add tbl k) v; true in
  let inp = Unix.open_process_in ("ocamldep "^args) in
  while add_dep (read_dep inp) do (); done;
  match Unix.close_process_in inp with
    | Unix.WEXITED _ ->
      transitive_closure ?equality:None ~norm:dedup ~op:(Hashtbl.find_all tbl)
    | _ ->
      failwith "OCamlDep exited abnormally"
  
