structure Input =
struct

  val removeCarriage =
    String.implode o (List.filter (fn c => c <> #"\r")) o String.explode

  (* getLines : string -> string list *)
  fun getLines name =
    let
      val ins = TextIO.openIn name
      fun loop s =
        case TextIO.inputLine s of
          SOME line => removeCarriage (String.substring (line, 0, String.size line - 1)) :: (loop s)
        | NONE      => []
    in
      loop ins before TextIO.closeIn ins
    end

  (* mapLines : string -> (string -> 'a) -> 'a list *)
  fun mapLines name f =
    let
      val ins = TextIO.openIn name
      fun loop s =
        case TextIO.inputLine s of
          SOME line => (f (String.substring (line, 0, String.size line - 1))) :: (loop s)
        | NONE      => []
    in
      loop ins before TextIO.closeIn ins
    end

fun until f [] = 0
  | until f (x::xs) = if f x then 0 else 1 + until f xs 

(* Split a list into a list list
 * split : ('a -> bool) -> 'a list -> 'a list list *)
fun split f [] = []
  | split f (x::xs) =
  if f x then split f xs else 
  let
    val n = until f (x::xs)
  in
    (List.take (x::xs, n)) :: 
    split f (List.drop (x::xs, n))
  end





end




