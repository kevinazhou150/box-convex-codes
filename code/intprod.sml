type codeword = int list
type code = codeword list

val () = Control.Print.printDepth := 100
val () = Control.Print.printLength := 10000

infix |>
fun x |> f = f x


(* toListList : 'a seq seq -> 'a list list *)
fun toListList S =
  S |> Seq.toList |> map Seq.toList


fun intsort L =
let
  fun intcmp (x,y) = (Int.compare (x,y) = GREATER)
in
  ListMergeSort.sort intcmp L
end


(* unique : ('a * 'a -> order) -> 'a list -> 'a list *)
(* also sorts the list *)
fun unique cmp L =
let
  fun unique' [] = []
    | unique' [x] = [x]
    | unique' (x::y::xs) =
    if cmp(x,y) = EQUAL then unique' (y::xs)
             else x::(unique' (y::xs))

  fun gt (x,y) = ( cmp(x,y) = GREATER )
in
  unique' (ListMergeSort.sort gt L)
end

(* uniqueS : ('a * 'a -> order) -> 'a seq -> 'a seq *)
fun uniqueS cmp S =
     S 
  |> Seq.toList
  |> unique cmp
  |> Seq.fromList


(* Implement an ordering for codewords *)
(* First order by length of codeword *)
(* Then order lexicographically *)
(* The codewords should be sorted *)
fun lexcmp cmp (L1, L2) =
  case Int.compare (List.length L1, List.length L2) of
       LESS => GREATER
     | GREATER => LESS
     | EQUAL =>
  let (* do lexicographic comparison *)
    fun lex ([], []) = EQUAL
      | lex ([], _) = raise Fail "should not happen"
      | lex (_, []) = raise Fail "should not happen"
      | lex (x::xs, y::ys) = 
    case cmp (x,y) of
      LESS => LESS
    | GREATER => GREATER
    | EQUAL => lex (xs, ys)
  in
    lex (L1, L2)
  end

(* Compares two codes.
 * Overall, use lexicographical sorting for all the codewords.
 * To compare individual codewords, use lexcmp.
 *)
fun codecmp cmp (C1, C2) =
  lexcmp (lexcmp cmp) (C1, C2)


(* e.g. permute [0,1,2,3] <2,3,0,1> ==>* [2,3,0,1]
 * the sequence P tells: 0 becomes 2, 1 becomes 3, 2 becomes 0, 3 becomes 1.
 *)
fun permute L P =
  map (fn x => Seq.nth P x) L

(* insperm : int seq -> int -> int -> int seq
 * sequence, position to insert, integer to insert *)
fun insperm p x n =
  Seq.append(Seq.take p x, Seq.cons n (Seq.drop p x))


(* generates all n! permutations
 * notably the permutations are 0-indexed.
 * permutations 3 = <<0,1,2>,<0,2,1>,<1,0,2>,<1,2,0>,<2,0,1>,<2,1,0>>
 * maybe in the wrong order *)
fun permutations 1 = Seq.singleton (Seq.singleton 0)
  | permutations n = 
     permutations (n-1)
  |> Seq.map (fn p => Seq.tabulate (fn x => insperm p x (n-1)) n)
  |> Seq.flatten


(* replace1 : int list -> int seq -> int list *)
fun replace1 L S =
let
  fun replaceH [] = []
    | replaceH (x::xs) =
      (Seq.nth S x) :: replaceH xs
in
  intsort (replaceH L)
end

(* replace2 : int list list -> int seq -> int list list *)
fun replace2 LL S = 
     LL
  |> map (fn L => replace1 L S)
  |> ListMergeSort.sort (fn (x,y) => lexcmp Int.compare (x,y) = GREATER)

(* n: # of neurons
 * C1, C2 : int list list (and are both sorted)
 * could make this parallel
 *)
fun checkIso n (C1, C2) = 
  let
    val perms = Seq.toList (permutations n) (* Better to just stage this
    computation *)
    fun checkPerms [] = false
      | checkPerms (p::ps) =
      replace2 C1 p = C2 orelse checkPerms ps
  in
    checkPerms perms
  end


(* Removes isomorphic codes.
 * Best run when given a collection of codes with the same number of
 * codewords of each degree.
 *)
fun removeIso1 n [] = []
  | removeIso1 n (c::cs) =
  let
    fun checkAll [] = false
      | checkAll (x::xs) =
      checkIso n (c,x) orelse checkAll xs
  in
    if checkAll cs then removeIso1 n cs
    else c :: (removeIso1 n cs)
  end


(* Int.compare except reversed *)
(* Use if want largest numbers first *)
fun smallintcmp (a, b) =
  case Int.compare (a, b) of
    LESS => GREATER
  | GREATER => LESS
  | EQUAL => EQUAL


(* REQUIRES: the codewords are sorted *)
fun intersection ([], _) = []
  | intersection (_, []) = []
  | intersection (x::xs, y::ys) = 
    case Int.compare (x,y) of
      EQUAL => x :: intersection (xs, ys)
    | LESS => intersection (xs, y::ys)
    | GREATER => intersection (x::xs, ys)

fun prodMap f ([], _) = []
  | prodMap f (_, []) = []
  | prodMap f ([x], y::ys) = f (x,y) :: prodMap f ([x], ys)
  | prodMap f (x::xs, y::ys) = prodMap f ([x], y::ys) 
                            @  prodMap f (xs, y::ys)

(* REQUIRES: each codeword of the codes are sorted *)
fun intprod (C1, C2) = prodMap intersection (C1, C2)

(* intprodAll : int list list list * int list list list 
*            -> int list list list *)
fun intprodAll (Cs1, Cs2) = prodMap intprod (Cs1, Cs2)


(* Generate all 1D codes on n neurons *)
(* First, given n intervals, find the code it generates *)
(* We take intervals to be closed. It suffices to consider a point's behavior at
 * every multiple of 0.5.
 *
 * Further, we only consider intervals with endpoints at 0, 1, 2, ..., 2n.
 *)

(* [0,2], [1,4], [2,3] *)
(* -----
 *   -------
 *     ---
 * 0 1 2 3 4
 *)

(* REQUIRES: a <= b *)
fun interval2bin (a,b) n = 
let
  fun interval2bin' (a,b) acc =
    if acc > n then [] 
    else if a <= acc andalso acc < b then 1 :: 1 :: interval2bin' (a,b) (acc+1)
    else if a <= acc andalso acc = b then 1 :: 0 :: interval2bin' (a,b) (acc+1)
    else 0 :: 0 :: interval2bin' (a,b) (acc+1)
in
  interval2bin' (a,b) 0
end

(* intervals2bin : (int * int) list -> int -> int list list *)
fun intervals2bin L n =
  map (fn p => interval2bin p n) L

(* only for rectangular matrix *)
fun transpose M =
  Seq.tabulate (fn i =>
    Seq.tabulate (fn j =>
      Seq.nth (Seq.nth M j) i
      ) (Seq.length M)
    ) (Seq.length (Seq.nth M 0))



(* bits2codeword : int seq -> int seq 
 * e.g. <0,0,1,1,0> becomes <2,3>
 *)
fun bits2codeword S =
    S
  |> Seq.enum
  |> Seq.filter (fn (i,x) => x = 1)
  |> Seq.map (fn (i,x) => i)

(* matrix2code : int seq seq -> int seq seq *)
fun matrix2code M = 
  M
  |> transpose
  |> Seq.map bits2codeword  


fun makeIntervals 1 = Seq.singleton (0,1)
  | makeIntervals n =
  Seq.append ( Seq.tabulate (fn i => (i,n)) n,
               makeIntervals (n-1))

(* listProd : 'a list list -> 'a list list
 * [[1,2,3,4,5],
 *  [3,4,5,6,7]]
 *  ==>* [[1,3], [1,4], [1,5], ... ]
 *)

fun listProd [] = [[]]
  | listProd (L::LL) = 
  List.concat (
    map (fn xs => 
    map (fn y => y::xs) L)
  (listProd LL)
  )

fun repeat 0 x = []
  | repeat n x = x :: repeat (n-1) x

(* creates n intervals with integer coordinates in [0,2n] *)
fun nIntervals n = 
     (2 * n - 1)
  |> makeIntervals
  |> Seq.toList
  |> repeat n
  |> listProd


fun deleteEmpty S =
  if Seq.null (Seq.nth S (Seq.length S - 1))
  then Seq.take S (Seq.length S - 1)
  else S 

(* unfortunately need to rewrite unique to use
 * sequences, to use deleteEmpty successfully *)
fun deleteEmptyL [] = []
  | deleteEmptyL [[]] = []
  | deleteEmptyL (x::xs) = x :: deleteEmptyL xs


fun intervals2code L n =
     intervals2bin L n
  |> Seq.fromList
  |> Seq.map Seq.fromList
  |> matrix2code
  |> Seq.toList
  |> map Seq.toList
  |> unique (lexcmp Int.compare)
  |> deleteEmptyL (* actually improves performance *)

(* Find all 1D codes on n neurons.
 * # of codes before using unique is 10000, becomes 1595
 * Then becomes ??? after getting rid of isomorphic codes
 *)
fun solve1DIso n =
     nIntervals n
  |> map (fn intervals => intervals2code intervals n)
  |> unique (codecmp Int.compare)


(* This sorting step is not needed because all the codes are already sorted *)
(* Although, we may want to sort in a different way *)
(*  |> ListMergeSort.sort (fn (x,y) => (codecmp Int.compare (x,y)) = GREATER) *)

(* for n=4, the removing iso part takes about 1/4 the time as the above part *)

fun solve1D n =
    removeIso1 n (solve1DIso n)


fun solve n d =
  let
    val ans1 = solve1DIso n
    fun help 1 = ans1
      | help d' = 
         intprodAll (ans1, help (d' -1))
      |> map (unique (lexcmp Int.compare))
      |> map deleteEmptyL
      |> unique (codecmp Int.compare)
      |> removeIso1 n
  in
    help d
  end


(* Uses pre-calculated data *)
fun solve1 n d =
  let
    val ans1 = data n
    val ans1Iso = dataIso n

    fun help 1 = ans1
      | help d' = 
         intprodAll (ans1, help (d' -1))
      |> map (unique (lexcmp Int.compare))
      |> map deleteEmptyL
      |> unique (codecmp Int.compare)
      |> removeIso1 n
  in
    help d
  end





fun codewordToString cw = List.foldr (fn (i, s) => Int.toString i ^ s) "" cw

fun codeToString c = 
  let
    val temp = List.foldr (fn (cw, s) => codewordToString cw ^ ", " ^ s) "" c
  in
    temp ^ "∅"
  end

fun printCodes cs =
  let
    fun printCode s = print (s ^ "\n")
  in
    map (printCode o codeToString) cs
  end


(* Known bugs / todos *)
(* on 2 indices it doesn't include the empty code *)
(* Implement collect-style removal of isomorphic codes *)
(* speed up generation of intervals, or intersection product-ing *)
(* Use interval stabbing (treaps / 210) to determine codewords *)





