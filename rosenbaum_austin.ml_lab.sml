(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Austin Rosenbaum
* austinjrosenbaum@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)

datatype 'element set =
		Empty
	| Set of 'element * 'element set;
(* Working Set *)
val a = Set(#"a", Set(#"b", Set(#"d", Empty)));
val b = Set(1, Empty)

(* Takes an element and a variable of type set, and will return true if the element is in the set, and false if the element is not in the set *)
fun isMember e Empty = false
	|	isMember e (Set(element, set)) =
		if e = element then true
		else isMember e set;

(* Folds the list using a helper function that adds the value to the set if it is not already a member *)
fun list2Set [] = Empty
	|	list2Set L = foldr (fn (e, es) =>
													if not (isMember e es) then Set(e, es)
													else es)
													Empty L;


(* The first pattern explicitly handles the case of empty sets by returning an empty set

The second two patterns take care of the cases of either set being empty by simply returning the non-empty set

Recurse through the second set entered until it is only one element and Empty, then if it is not part of the first set, return a set with the element added. If it is a member, skip the element and simply return the first set. *)
fun union Empty Empty = Empty
	|	union Empty s = s
	|	union s Empty = s
	| union (Set(e1, es1)) (Set(e2, es2)) =
			if es2 = Empty then
				if not (isMember e2 (Set(e1, es1))) then Set(e2, (Set(e1, es1)))
				else Set(e1, es1)
			else union (Set(e1, es1)) es2;

(* Recurse through one of the sets, checking each element against the other set. If the element is in both sets, create a set with it and an Empty element. Continue adding to this new set only if the next element is in the first set and not already in the new set. *)
fun intersect Empty s = Empty
	| intersect s Empty = Empty
	| intersect (Set(e1, es1)) (Set(e2, es2)) =
		if es2 = Empty then
			if (isMember e2 (Set(e1, es1))) then Set(e2, Empty)
			else Empty
		else
			if (isMember e2 (Set(e1, es1))) andalso not (isMember e2 (intersect (Set(e1, es1)) es2)) then
				Set(e2, (intersect (Set(e1, es1))) es2)
			else intersect (Set(e1, es1)) es2;


(* fun list2Set [] = Empty
	|	list2Set L	=
			if (tl L) = nil then Set((hd L), Empty)
			else
				if not (isMember (hd L) (list2Set (tl L))) then Set((hd L), (list2Set (tl L)))
				else list2Set (tl L); *)

isMember #"a" a;
isMember #"c" a;
isMember 1 b;
isMember 2 b;

fun f [] = [] (*a. If the list is empty, return an empty list *)
	| f (x::xs) = (x + 1) :: (f xs); (*b. Add 1 to the head, recursively call function on the list's tail. *)


(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

stringifyCharSet a;

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
(* isMember "one" (list2Set ["1", "2", "3", "4"]); *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
(* list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"]; *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
val quest9 = union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]);
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
val quest10 = intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]);
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
