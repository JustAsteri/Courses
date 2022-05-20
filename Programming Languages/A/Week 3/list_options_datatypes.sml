datatype intoption = NULL
		   | SOME of int
				 

fun inc_or_zero intoption =
    case intoption of
	NULL => 0
     | SOME i => i+1

fun sum_list xs =
    case xs of
	[] => 0
      | x::xs' => x + sum_list xs' (* 1 :: (2 :: (3 :: []) = 1 + (2 + (3 + 0)) = 6 *)
			       
fun append_list (xs, ys) =
    case xs of
	[] => ys
      | x::xs' => x :: append_list(xs',ys) (* 1 :: (2 :: (3 :: []) = 1 :: (2 :: [3]) = 1 :: [2,3] = [1,2,3] *
