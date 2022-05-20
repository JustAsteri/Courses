datatype 'a option = NONE | SOME of 'a

datatype 'a mylist = EMPTY | Cons of 'a * 'a mylist

datatype ('a,'b) tree =
	 Node of 'a * ('a, 'b) tree * ('a, 'b) tree
	 | Leaf of 'b

fun sum_tree tr =
    case tr of
	Leaf i => i
      | Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

fun sum_leaves tr =
    case tr of
	Leaf i => i
      | Node(i, lft, rgt) => sum_leaves lft + sum_leaves rgt

fun num_leaves tr =
    case tr of
	Leaf i => 1
     | Node(i, lft, rgt) => num_leaves lft + num_leaves rgt

val tree = Node(1, Leaf 5, Node(2, Leaf 3, Leaf 4))
(* 
    4
   /
  2
 / \
1   3
 \
  5
*)

val fiftteen = sum_tree(tree);
val twelve = sum_leaves(tree);
val three = num_leaves(tree);
