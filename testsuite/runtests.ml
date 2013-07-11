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

module type UTF = sig
  (** read a character at a position in memory, return the character and
      the next read position. *)
  val get : string -> int -> character * int

  (** write a character to memory in the transfer format; return the
      next write position. *)
  val set : string -> int -> character -> int
end


(*************************************************
 * :: UTF-8
 *************************************************)

module UTF_8 : UTF = struct

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
 * :: UTF-16
 *************************************************)

module UTF_16BE : UTF = struct

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


module UTF_16LE : UTF = struct

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
 * :: UTF-32
 *************************************************)

module UTF_32BE : UTF = struct

  let get s i =
    let cp =
      UChar.chr (
        Char.code s.[i + 0] lsl (8 * 3) +
        Char.code s.[i + 1] lsl (8 * 2) +
        Char.code s.[i + 2] lsl (8 * 1) +
        Char.code s.[i + 3] lsl (8 * 0)
      )
    in
    (cp, i + 4)


  let set s i cp =
    let cp = UChar.code cp in
    s.[i + 0] <- Char.chr (mask8 (cp lsr (8 * 3)));
    s.[i + 1] <- Char.chr (mask8 (cp lsr (8 * 2)));
    s.[i + 2] <- Char.chr (mask8 (cp lsr (8 * 1)));
    s.[i + 3] <- Char.chr (mask8 (cp lsr (8 * 0)));
    i + 4

end


module UTF_32LE : UTF = struct

  let get s i =
    let c =
      UChar.chr (
        Char.code s.[i + 0] lsl (8 * 0) +
        Char.code s.[i + 1] lsl (8 * 1) +
        Char.code s.[i + 2] lsl (8 * 2) +
        Char.code s.[i + 3] lsl (8 * 3)
      )
    in
    (c, i + 4)


  let set s i c =
    let c = UChar.code c in
    s.[i + 0] <- Char.chr (mask8 (c lsr (8 * 0)));
    s.[i + 1] <- Char.chr (mask8 (c lsr (8 * 1)));
    s.[i + 2] <- Char.chr (mask8 (c lsr (8 * 2)));
    s.[i + 3] <- Char.chr (mask8 (c lsr (8 * 3)));
    i + 4

end


(*************************************************
 * :: UTF-N String
 *************************************************)

module type UtfString = sig
  type t

  val data : t -> string

  (** get a character; may be O(n) depending on transfer format *)
  val get : t -> int -> character

  (** set a character; may be O(n) depending on transfer format *)
  val set : t -> int -> character -> unit

  (** [create n] makes a new string with [n] code units *)
  val create : int -> t
  (** [copy s] makes a new string with the same contents as [s] *)
  val copy : t -> t

  (** [concat s l] joins a string list [l] with separator [s] *)
  val concat : t -> t list -> t

  (** number of characters in the string *)
  val length : t -> int

  (** substring of characters *)
  val sub : t -> int -> int -> t

  (** [fill_units s start len c] fills the string with [len]
      characters [c] starting at character [start]. *)
  val fill : t -> int -> int -> character -> unit

  val make : int -> character -> t
  val blit : t -> int -> t -> int -> int -> unit
  val iter : (character -> unit) -> t -> unit
  val iteri : (int -> character -> unit) -> t -> unit
  val map : (character -> character) -> t -> t
  val mapi : (int -> character -> character) -> t -> t
  val index : t -> character -> int
  val index_from : t -> int -> character -> int
  val rindex : t -> character -> int
  val rindex_from : t -> int -> character -> int
  val contains_from : t -> int -> character -> bool
  val contains : t -> character -> bool
  val rcontains_from : t -> int -> character -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val fold_left : ('a -> character -> 'a) -> 'a -> t -> 'a
  val fold_lefti : (int -> 'a -> character -> 'a) -> 'a -> t -> 'a
  val fold_right : (character -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_righti : (int -> character -> 'a -> 'a) -> t -> 'a -> 'a
end


module Utf32String : UtfString = struct
  type t = string

  let data t = t

  let get : t -> int -> character =
  fun s i ->
    let c0 = Char.code s.[i * 4 + 0] in
    let c1 = Char.code s.[i * 4 + 1] in
    let c2 = Char.code s.[i * 4 + 2] in
    let c3 = Char.code s.[i * 4 + 3] in
    UChar.chr (
      c0 lsl (8 * 3) +
      c1 lsl (8 * 2) +
      c2 lsl (8 * 1) +
      c3 lsl (8 * 0)
    )


  let set : t -> int -> character -> unit =
  fun s i c ->
    let c = UChar.code c in
    s.[i * 4 + 0] <- Char.chr (mask8 (c lsr (8 * 3)));
    s.[i * 4 + 1] <- Char.chr (mask8 (c lsr (8 * 2)));
    s.[i * 4 + 2] <- Char.chr (mask8 (c lsr (8 * 1)));
    s.[i * 4 + 3] <- Char.chr (mask8 (c lsr (8 * 0)));
  ;;


  let create : int -> t =
  fun n ->
    String.create (n * 4)


  let copy : t -> t = String.copy
  let concat : t -> t list -> t = String.concat


  let length : t -> int =
  fun s ->
    let len = String.length s in
    assert (len mod 4 = 0);
    len / 4


  let sub : t -> int -> int -> t =
  fun s start len ->
    String.sub s (start * 4) (len * 4)


  let fill : t -> int -> int -> character -> unit =
  fun s start len c ->
    for i = start to start + len do
      set s i c
    done


  let make : int -> character -> t =
  fun n c ->
    let s = create n in
    fill s 0 (n - 1) c;
    s


  let blit : t -> int -> t -> int -> int -> unit =
  fun src srcoff dst dstoff len ->
    String.blit src (srcoff * 4) dst (dstoff * 4) (len * 4)


  let iter : (character -> unit) -> t -> unit =
  fun f s ->
    for i = 0 to length s - 1 do
      f (get s i)
    done


  let iteri : (int -> character -> unit) -> t -> unit =
  fun f s ->
    for i = 0 to length s - 1 do
      f i (get s i)
    done


  let map : (character -> character) -> t -> t =
  fun f s ->
    let mapped = create (length s) in
    for i = 0 to length s - 1 do
      set mapped i (f (get s i))
    done;
    mapped


  let mapi : (int -> character -> character) -> t -> t =
  fun f s ->
    let mapped = create (length s) in
    for i = 0 to length s - 1 do
      set mapped i (f i (get s i))
    done;
    mapped


  let rec index_rec s lim i c =
    if i >= lim then
      raise Not_found
    else if get s i = c then
      i
    else
      index_rec s lim (i + 1) c

  let index : t -> character -> int =
  fun s c ->
    index_rec s (length s) 0 c

  let index_from : t -> int -> character -> int =
  fun s i c ->
    let l = length s in
    if i < 0 || i > l then
      invalid_arg "Utf32.index_from"
    else
      index_rec s l i c


  let rec rindex_rec s i c =
    if i < 0 then
      raise Not_found
    else if get s i = c then
      i
    else
      rindex_rec s (i - 1) c

  let rindex : t -> character -> int =
  fun s c ->
    rindex_rec s (length s - 1) c

  let rindex_from : t -> int -> character -> int =
  fun s i c ->
    if i < -1 || i >= length s then
      invalid_arg "Utf32.rindex_from"
    else
      rindex_rec s i c


  let contains_from : t -> int -> character -> bool =
  fun s i c ->
    let l = length s in
    if i < 0 || i > l then
      invalid_arg "Utf32.contains_from"
    else
      try
        ignore (index_rec s l i c);
        true
      with Not_found ->
        false


  let contains : t -> character -> bool =
  fun s c ->
    contains_from s 0 c

  let rcontains_from : t -> int -> character -> bool =
  fun s i c ->
    if i < 0 || i > length s then
      invalid_arg "Utf32.rcontains_from"
    else
      try
        ignore (rindex_rec s i c);
        true
      with Not_found ->
        false


  let compare : t -> t -> int = Pervasives.compare
  let equal : t -> t -> bool = Pervasives.(=)


  let rec fold_left i f x s =
    if length s - i > 0 then
      fold_left (i + 1) f (f x (get s i)) s
    else
      x

  let fold_left : 'a. ('a -> character -> 'a) -> 'a -> t -> 'a =
  fun f x s ->
    fold_left 0 f x s


  let rec fold_lefti i f x s =
    if length s - i > 0 then
      fold_lefti (i + 1) f (f i x (get s i)) s
    else
      x

  let fold_lefti : 'a. (int -> 'a -> character -> 'a) -> 'a -> t -> 'a =
  fun f x s ->
    fold_lefti 0 f x s


  let rec fold_right i f s x =
    if i >= 0 then
      fold_right (i - 1) f s (f (get s i) x)
    else
      x

  let fold_right : 'a. (character -> 'a -> 'a) -> t -> 'a -> 'a =
  fun f s x ->
    fold_right (length s - 1) f s x


  let rec fold_righti i f s x =
    if i >= 0 then
      fold_righti (i - 1) f s (f i (get s i) x)
    else
      x

  let fold_righti : 'a. (int -> character -> 'a -> 'a) -> t -> 'a -> 'a =
  fun f s x ->
    fold_righti (length s - 1) f s x


end


let _ =
  let s = Utf32String.make 10 (UChar.chr 0x263a) in
  String.iter (fun c ->
    Printf.printf "%02x " (Char.code c)
  ) (Utf32String.data s);
  print_endline "--";
  Utf32String.iter (fun c ->
    Printf.printf "%x\n" (UChar.code c)
  ) s;
  Utf32String.fold_left (fun n c ->
    Printf.printf "%d: %x\n" n (UChar.code c);
    n + 1
  ) 0 s;
  Utf32String.fold_lefti (fun i n c ->
    Printf.printf "%d = %d: %x\n" i n (UChar.code c);
    n + 1
  ) 0 s;
  Utf32String.fold_right (fun c n ->
    Printf.printf "%d: %x\n" n (UChar.code c);
    n + 1
  ) s 0;
  Utf32String.fold_righti (fun i c n ->
    Printf.printf "%d = %d: %x\n" i n (UChar.code c);
    n + 1
  ) s 0;
;;
