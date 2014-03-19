open Str

(** dedup xs returns a list of the elements of xs *)
let rec dedup ?equality:(eq=(=)) = function
  | [] -> []
  | hd::tl -> 
    hd::(dedup (List.filter (fun x -> not (eq hd x)) tl))

let rec fixpoint ?equality:((=)=(=)) endo x = 
  let endo_x = endo x in
  if endo_x = endo endo_x then 
    endo_x 
  else
    fixpoint endo endo_x

let shadow k v f k' =
  if k = k' then v else f k

(** split_path f returns the tuple of path, basename, and extension of f *)
let split_path (f: string): string * string * string =
  let split = try String.rindex f '/' with Not_found -> -1 in
  let path = string_before f (succ split)
  and filename = 
    if split = -1 then f else string_after f (succ split) in
  let split = 
    try String.rindex filename '.' with Not_found -> 0 in
  let basename = string_before filename split
  and extension = string_after filename split in
  (path, basename, extension)

let basename f = let (_,b,_) = split_path f in b
