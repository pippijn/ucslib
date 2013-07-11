(* A Unicode character/code point *)
type character = UChar.t


(*************************************************
 * :: Unicode Transfer Format interface
 *************************************************)

module type Scheme = sig
  val name : string

  (** read a character at a position in memory, return the character
      and the number of bytes written. *)
  val get : string -> int -> character * int

  (** write a character to memory in the transfer format; return the
      number of bytes written. *)
  val set : string -> int -> character -> int
end


(*************************************************
 * :: Immutable Unicode strings
 *************************************************)

module type String = sig
  type t = private string

  val unsafe_adopt : string -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val map : (character -> character) -> t -> t
  val mapi : (int -> character -> character) -> t -> t

  val fold_left : ('a -> character -> 'a) -> 'a -> t -> 'a
end


(*************************************************
 * :: Mutable Unicode buffers
 *************************************************)

module type Buffer = sig
  type t
  type data

  val create : int -> t

  val add_char : t -> character -> unit

  val contents : t -> data
end


(*************************************************
 * :: Collection of encoder modules
 *************************************************)

module type S = sig
  val name : string

  module String : String
  module Buffer : Buffer
    with type data = String.t
end
