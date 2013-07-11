open_out "testsuite.rst";;


(*************************************************
 * :: Unicode constants
 *************************************************)

let lead_surrogate_min	= 0xd800
let lead_surrogate_max	= 0xdbff
let trail_surrogate_min	= 0xdc00
let trail_surrogate_max	= 0xdfff
let lead_offset		= lead_surrogate_min - (0x10000 lsr 10)
let surrogate_offset	= 0x10000 - (lead_surrogate_min lsr 10) - trail_surrogate_min

let code_point_max	= 0x10ffff


(*************************************************
 * :: Helper functions
 *************************************************)

let mask8 = (land) 0xff
let mask16 = (land) 0xffff

let is_lead_surrogate code =
  code >= lead_surrogate_min && code <= lead_surrogate_max

let is_trail_surrogate code =
  code >= trail_surrogate_min && code <= trail_surrogate_max

let is_surrogate code =
  code >= lead_surrogate_min && code <= trail_surrogate_max

let is_trail_byte c =
  Char.code c lsr 6 = 2

let utf8_length c =
  let d = Char.code c in
  if d >= 0x00 && d < 0x80 then 1 else
  if d >= 0xc0 && d < 0xe0 then 2 else
  if d >= 0xe0 && d < 0xf0 then 3 else
  if d >= 0xf0 && d < 0xf8 then 4 else
  if d >= 0xf8 && d < 0xfc then 5 else
  if d >= 0xfc && d < 0xfe then 6 else
  invalid_arg "utf8_length"



(*************************************************
 * :: Unicode character/code point
 *************************************************)

module UChar : sig
  type t = private int

  val chr : int -> t
  val code : t -> int

end = struct
  type t = int

  let chr cp = cp
  let code cp = cp
end


type character = UChar.t


(*************************************************
 * :: Unicode Transfer Format interface
 *************************************************)

module type UTF_Scheme = sig
  (** read a character at a position in memory, return the character
      and the number of bytes written. *)
  val get : string -> int -> character * int

  (** write a character to memory in the transfer format; return the
      number of bytes written. *)
  val set : string -> int -> character -> int
end


(*************************************************
 * :: UTF-8
 *************************************************)

module UTF_8_Scheme : UTF_Scheme = struct

  let get s i =
    let length = utf8_length s.[i] in

    let cp =
      match length with
      | 1 ->
          ((Char.code s.[i + 0] lsl (6 * 0)) land 0x000000ff)
      | 2 ->
          ((Char.code s.[i + 1] lsl (6 * 0)) land 0x0000003f) +
          ((Char.code s.[i + 0] lsl (6 * 1)) land 0x000007ff)
      | 3 ->
          ((Char.code s.[i + 2] lsl (6 * 0)) land 0x0000003f) +
          ((Char.code s.[i + 1] lsl (6 * 1)) land 0x00000fff) +
          ((Char.code s.[i + 0] lsl (6 * 2)) land 0x0000ffff)
      | 4 ->
          ((Char.code s.[i + 3] lsl (6 * 0)) land 0x0000003f) +
          ((Char.code s.[i + 2] lsl (6 * 1)) land 0x00000fff) +
          ((Char.code s.[i + 1] lsl (6 * 2)) land 0x0003ffff) +
          ((Char.code s.[i + 0] lsl (6 * 3)) land 0x001fffff)
      (* FIXME: I don't know whether these are correct (probably not). *)
      | 5 ->
          ((Char.code s.[i + 4] lsl (6 * 0)) land 0x0000003f) +
          ((Char.code s.[i + 3] lsl (6 * 1)) land 0x00000fff) +
          ((Char.code s.[i + 2] lsl (6 * 2)) land 0x0000ffff) +
          ((Char.code s.[i + 1] lsl (6 * 3)) land 0x001fffff) +
          ((Char.code s.[i + 0] lsl (6 * 4)) land 0x03ffffff)
      | 6 ->
          ((Char.code s.[i + 5] lsl (6 * 0)) land 0x0000003f) +
          ((Char.code s.[i + 4] lsl (6 * 1)) land 0x00000fff) +
          ((Char.code s.[i + 3] lsl (6 * 2)) land 0x0000ffff) +
          ((Char.code s.[i + 2] lsl (6 * 3)) land 0x001fffff) +
          ((Char.code s.[i + 1] lsl (6 * 4)) land 0x03ffffff) +
          ((Char.code s.[i + 0] lsl (6 * 5)) land 0x7fffffff)
      | _ -> assert false
    in

    (UChar.chr cp, length)


  let set s i cp =
    let cp = UChar.code cp in
    let continuation n = ((cp lsr (6 * n)) land 0x3f) lor 0x80 in
    let leading n mask =  (cp lsr (6 * n))            lor mask in

    if cp <= 0x007f then (
      (* one octet *)
      s.[i + 0] <- char_of_int cp;
      1
    ) else if (cp <= 0x07ff) then (
      (* two octets *)
      s.[i + 0] <- char_of_int (leading 1 0xc0);
      s.[i + 1] <- char_of_int (continuation 0);
      2
    ) else if (cp <= 0xffff) then (
      (* three octets *)
      s.[i + 0] <- char_of_int (leading 2 0xe0);
      s.[i + 1] <- char_of_int (continuation 1);
      s.[i + 2] <- char_of_int (continuation 0);
      3
    ) else if (cp <= 0x1fffff) then (
      (* four octets *)
      s.[i + 0] <- char_of_int (leading 3 0xf0);
      s.[i + 1] <- char_of_int (continuation 2);
      s.[i + 2] <- char_of_int (continuation 1);
      s.[i + 3] <- char_of_int (continuation 0);
      4
    ) else if (cp <= 0x3ffffff) then (
      (* five octets *)
      s.[i + 0] <- char_of_int (leading 4 0xf8);
      s.[i + 1] <- char_of_int (continuation 3);
      s.[i + 2] <- char_of_int (continuation 2);
      s.[i + 3] <- char_of_int (continuation 1);
      s.[i + 4] <- char_of_int (continuation 0);
      5
    ) else if (cp <= 0x7fffffff) then (
      (* six octets *)
      s.[i + 0] <- char_of_int (leading 5 0xfc);
      s.[i + 1] <- char_of_int (continuation 4);
      s.[i + 2] <- char_of_int (continuation 3);
      s.[i + 3] <- char_of_int (continuation 2);
      s.[i + 4] <- char_of_int (continuation 1);
      s.[i + 5] <- char_of_int (continuation 0);
      6
    ) else (
      invalid_arg "UTF-8 encoding impossible"
    )

end


(*************************************************
 * :: UTF-16BE/LE
 *************************************************)

module UTF_16BE_Scheme : UTF_Scheme = struct

  let get s i =
    let lead =
      Char.code s.[i + 0] lsl (8 * 1) +
      Char.code s.[i + 1] lsl (8 * 0)
    in

    if not (is_lead_surrogate lead) then
      (UChar.chr lead, 2)
    else
      let trail =
        Char.code s.[i + 2] lsl (8 * 1) +
        Char.code s.[i + 3] lsl (8 * 0)
      in
      (UChar.chr (
        ((lead - 0xd800) * 0x400) + (trail - 0xdc00) + 0x10000
      ), 4)


  let set s i cp =
    let cp = UChar.code cp in
    if cp <= 0xffff then (
      s.[i + 0] <- Char.chr (mask8 (cp lsr (8 * 1)));
      s.[i + 1] <- Char.chr (mask8 (cp lsr (8 * 0)));
      2
    ) else (
      let lead  = mask16 ((cp lsr 10)     + lead_offset        ) in
      let trail = mask16 ((cp land 0x3ff) + trail_surrogate_min) in
      s.[i + 0] <- Char.chr (mask8 (lead  lsr (8 * 1)));
      s.[i + 1] <- Char.chr (mask8 (lead  lsr (8 * 0)));
      s.[i + 2] <- Char.chr (mask8 (trail lsr (8 * 1)));
      s.[i + 3] <- Char.chr (mask8 (trail lsr (8 * 0)));
      4
    )

end


module UTF_16LE_Scheme : UTF_Scheme = struct

  let get s i =
    let trail =
      Char.code s.[i + 0] lsl (8 * 0) +
      Char.code s.[i + 1] lsl (8 * 1)
    in

    if not (is_trail_surrogate trail) then
      (UChar.chr trail, 2)
    else
      let lead =
        Char.code s.[i + 2] lsl (8 * 0) +
        Char.code s.[i + 3] lsl (8 * 1)
      in
      (UChar.chr (
        ((lead - 0xd800) * 0x400) + (trail - 0xdc00) + 0x10000
      ), 4)


  let set s i cp =
    let cp = UChar.code cp in
    if cp <= 0xffff then (
      s.[i + 0] <- Char.chr (mask8 (cp lsr (8 * 0)));
      s.[i + 1] <- Char.chr (mask8 (cp lsr (8 * 1)));
      2
    ) else (
      let lead  = mask16 ((cp lsr 10)     + lead_offset        ) in
      let trail = mask16 ((cp land 0x3ff) + trail_surrogate_min) in
      s.[i + 0] <- Char.chr (mask8 (trail lsr (8 * 0)));
      s.[i + 1] <- Char.chr (mask8 (trail lsr (8 * 1)));
      s.[i + 2] <- Char.chr (mask8 (lead  lsr (8 * 0)));
      s.[i + 3] <- Char.chr (mask8 (lead  lsr (8 * 1)));
      4
    )

end


(*************************************************
 * :: UTF-32BE/LE
 *************************************************)

module UTF_32BE_Scheme : UTF_Scheme = struct

  let get s i =
    let cp =
      UChar.chr (
        Char.code s.[i + 0] lsl (8 * 3) +
        Char.code s.[i + 1] lsl (8 * 2) +
        Char.code s.[i + 2] lsl (8 * 1) +
        Char.code s.[i + 3] lsl (8 * 0)
      )
    in
    (cp, 4)


  let set s i cp =
    let cp = UChar.code cp in
    s.[i + 0] <- Char.chr (mask8 (cp lsr (8 * 3)));
    s.[i + 1] <- Char.chr (mask8 (cp lsr (8 * 2)));
    s.[i + 2] <- Char.chr (mask8 (cp lsr (8 * 1)));
    s.[i + 3] <- Char.chr (mask8 (cp lsr (8 * 0)));
    4

end


module UTF_32LE_Scheme : UTF_Scheme = struct

  let get s i =
    let c =
      UChar.chr (
        Char.code s.[i + 0] lsl (8 * 0) +
        Char.code s.[i + 1] lsl (8 * 1) +
        Char.code s.[i + 2] lsl (8 * 2) +
        Char.code s.[i + 3] lsl (8 * 3)
      )
    in
    (c, 4)


  let set s i c =
    let c = UChar.code c in
    s.[i + 0] <- Char.chr (mask8 (c lsr (8 * 0)));
    s.[i + 1] <- Char.chr (mask8 (c lsr (8 * 1)));
    s.[i + 2] <- Char.chr (mask8 (c lsr (8 * 2)));
    s.[i + 3] <- Char.chr (mask8 (c lsr (8 * 3)));
    4

end


(*************************************************
 * :: Immutable Unicode strings
 *************************************************)

module type UTF_String = sig
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

module type UTF_Buffer = sig
  type t
  type data

  val create : int -> t

  val add_char : t -> character -> unit

  val contents : t -> data
end


(*************************************************
 * :: Recursive definition of the above two
 *************************************************)

module type UTF = sig
  module UString : UTF_String
  module UBuffer : UTF_Buffer
    with type data = UString.t
end

module UTF(UTF : UTF_Scheme) : UTF = struct
  module rec UString : UTF_String = struct

    type t = string

    let unsafe_adopt s = s

    let compare = Pervasives.compare
    let equal = Pervasives.(=)

    let rec fold_left i f x s =
      if String.length s - i > 0 then
        let (cp, advance) = UTF.get s i in
        fold_left (i + advance) f (f x cp) s
      else
        x

    let fold_left f x s =
      fold_left 0 f x s

    let map f s =
      let mapped = UBuffer.create (String.length s) in
      fold_left (fun () cp ->
        UBuffer.add_char mapped (f cp)
      ) () s;
      UBuffer.contents mapped

    let mapi f s =
      let mapped = UBuffer.create (String.length s) in
      ignore (fold_left (fun i cp ->
        UBuffer.add_char mapped (f i cp);
        i + 1
      ) 0 s);
      UBuffer.contents mapped

  end
  and UBuffer : UTF_Buffer with type data = UString.t = struct

    type t = {
      mutable buf : string;
      mutable pos : int;
    }

    type data = UString.t

    let create len = {
      buf = String.make len '\xff';
      pos = 0;
    }

    let add_char b cp =
      (* Make sure at least 6 bytes fit in. That is
       * the maximum for UTF-8 encoded code points. *)
      if b.pos + 6 > String.length b.buf then (
        let buf = String.create (String.length b.buf * 2 + 6) in
        String.blit b.buf 0 buf 0 b.pos;
        b.buf <- buf;
      );
      b.pos <- b.pos + UTF.set b.buf b.pos cp

    let contents b =
      UString.unsafe_adopt (String.sub b.buf 0 b.pos)

  end
end


module UTF_8    = UTF(UTF_8_Scheme)
module UTF_16BE = UTF(UTF_16BE_Scheme)
module UTF_16LE = UTF(UTF_16LE_Scheme)
module UTF_32BE = UTF(UTF_32BE_Scheme)
module UTF_32LE = UTF(UTF_32LE_Scheme)


let utf_test utf =
  let module U = (val utf : UTF) in
  let open U in

  let buf = UBuffer.create 4 in
  UBuffer.add_char buf (UChar.chr 0x263a);
  UBuffer.add_char buf (UChar.chr 0x263a);
  UBuffer.add_char buf (UChar.chr 0x263a);
  UBuffer.add_char buf (UChar.chr 0x263a);

  let s = UBuffer.contents buf in

  String.iter (fun c ->
    Printf.printf "%02x " (Char.code c)
  ) (s :> string);
  print_endline "--";

  print_endline "fold_left";
  UString.fold_left (fun n cp ->
    Printf.printf "%d: %x\n" n (UChar.code cp);
    n + 1
  ) 0 s;

  print_endline "map";
  let s =
    UString.map (fun cp ->
      UChar.chr (UChar.code cp - 1)
    ) s
  in

  UString.fold_left (fun n cp ->
    Printf.printf "%d: %x\n" n (UChar.code cp);
    n + 1
  ) 0 s;

  ()


let _ =
  List.iter utf_test [
    (module UTF_8 : UTF);
    (module UTF_16BE : UTF);
    (module UTF_16LE : UTF);
    (module UTF_32BE : UTF);
    (module UTF_32LE : UTF);
  ]
