fun sum xs =
    case xs of
	[] => 0
      | x::xs' => x + sum xs'

fun sum_new xs =
    let fun aux(xs,acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs',x+acc)
    in
	aux(xs,0)
    end

fun rev xs =
    case xs of
	[] => []
      | x::xs' => (rev xs') @ [x]

fun rev_new xs =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	     | x::xs' => aux(xs',x::acc)
    in
	aux(xs, [])
    end
	
val x = sum([1,2,3])
val x_new = sum_new([1,2,3])
val y = rev([1,2,3])
	   val y_new = rev_new([1,2,3])
