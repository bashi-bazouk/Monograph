
module Tree = struct

  type 'a tree = Branch of 'a * 'a tree list | Leaf of 'a

end
