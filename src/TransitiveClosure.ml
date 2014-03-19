open List
open Util

let transitive_closure ?equality:(eq=(=)) ?norm:(norm=(fun x -> x)) ~op:op =
  let concat_map op (set: 'a list): 'a list = concat (map op set) in
  fixpoint ~equality:eq (fun xs -> norm (xs@(concat_map op xs)))
