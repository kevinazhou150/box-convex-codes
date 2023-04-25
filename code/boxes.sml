
(* ccons is curried cons *)
fun ccons x codeword = x::codeword

(* makeBinary 2 = [[0,0],[0,1],[1,0],[1,1]
 * makeBinary : int -> int list list
 *)
fun makeBinary 0 = [[]]
  | makeBinary n = 
  let
    val prev = makeBinary (n-1)
  in
      (map (ccons 0) prev)
    @ (map (ccons 1) prev)
  end

type interval = int * int
type box = interval list
type codeword = int list
type code = codeword list (* set of codewords *)

(* SETTINGS 
 * max: the maximum coordinate
 * dimension and # neurons are the other parameters
 *)

(* total # possible boxes is (max+1)^(2 * dimension) *)

fun upto n =
  let
    fun help 0 acc = acc
      | help n acc = help (n-1) (n::acc)
  in
    help n [0]
  end

fun prod [] B = []
  | prod A [] = []
  | prod [x] (y::ys) = (x,y)::(prod [x] ys)
  | prod (x::xs) (y::ys) = 
  (prod [x] (y::ys)) @ (prod xs (y::ys))

fun allBoxesDim2 max = 
  let
    val bracketmax = upto max
    val intervals = prod bracketmax bracketmax
  in
    prod intervals intervals
  end

i 
  (* doesn't typecheck since prods aren't lists
fun allBoxes max 1 = prod (upto max) (upto max)
  | allBoxes max n =
  prod (allBoxes max 1) (allBoxes max (n-1))
  *)

fun findCodes max dimension neurons = raise Fail "unimplemented" 





val test = fn () => findCodes 2 2 3

