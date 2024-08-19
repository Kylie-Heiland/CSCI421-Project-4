(* This project multiplies integers, appends, and reverses elements that are of the mylist datatype. 
  Kylie Heiland
  CSCI421_VA
  4/11/23
*)

datatype 'element mylist = NIL | CONS of 'element * 'element mylist;

(*Returns the product of all integers in a mylist of integers*)
fun prod NIL = 1 
  | prod (CONS(head, tail)) = head * prod tail;

(*Returns a myList containing all values from a, followed by all values from b*)
fun append NIL b = b 
  | append (CONS(a, b)) c = CONS(a, append b c);

(*Returns a mylist in reverse order*)
fun reverse NIL = NIL
  | reverse (CONS(head, tail)) = append (reverse tail) (CONS(head, NIL));

infixr 5 CONS;

val a = 1 CONS 2 CONS 3 CONS NIL;

val b = 4 CONS 5 CONS NIL;

val c = append a b;

val d = prod c;

val e = reverse c;