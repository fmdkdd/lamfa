(* type num = { n: int } *)

(* module type NUM = sig *)
(*     val create : int -> num *)
(*     val eval : num -> int *)
(* end *)

type numt = < create : int -> < eval : int > >

let num = object
  method create n = object
      val n = n
      method eval = n
    end
  end

module Plus =
  struct
    let create l r = object
        val l = l
        val r = r
        method eval = l#eval + r#eval
      end
  end

module Show =
  functor (Num : numt) ->
  struct
    let num = object(self)
        method show = string_of_int self#n
      end
  end

(* Example of using functors *)
(* from http://www.cs.cornell.edu/Courses/cs3110/2011sp/lectures/lec09-functors/functors.htm  *)

module type EQUAL =
  sig
   type t
   val equal : t -> t -> bool
  end

module type SETFUNCTOR =
  functor (Equal : EQUAL) ->
  sig
    type elt = Equal.t
    type set
    val empty : set
    val mem : elt -> set -> bool
    val add : elt -> set -> set
    val size : set -> int
  end

module MakeSet : SETFUNCTOR =
  functor (Equal : EQUAL) ->
  struct
    open Equal
    type elt = t
    type set = elt list
    let empty = []
    let mem x = List.exists (equal x)
    let add x s = if mem x s then s else x :: s
    let size = List.length
  end

module StringNoCase =
  struct
  type t = string
  let equal s1 s2 =
    String.lowercase s1 = String.lowercase s2
  end

module SSet = MakeSet (StringNoCase)
