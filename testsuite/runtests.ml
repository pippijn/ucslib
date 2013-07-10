open_out "testsuite.rst";;


module UChar : sig
  type t = private int

  val chr : int -> t
  val code : t -> int

end = struct
  type t = int

  let chr cp = cp
  let code cp = cp
end


module Utf32 = struct
  type t = string

  let get : t -> int -> UChar.t =
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


  let set : t -> int -> UChar.t -> unit =
  fun s i c ->
    let c = UChar.code c in
    s.[i * 4 + 0] <- Char.chr ((c lsr (8 * 3)) land 0xff);
    s.[i * 4 + 1] <- Char.chr ((c lsr (8 * 2)) land 0xff);
    s.[i * 4 + 2] <- Char.chr ((c lsr (8 * 1)) land 0xff);
    s.[i * 4 + 3] <- Char.chr ((c lsr (8 * 0)) land 0xff);
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


  let fill : t -> int -> int -> UChar.t -> unit =
  fun s start len c ->
    for i = start to start + len do
      set s i c
    done


  let make : int -> UChar.t -> t =
  fun n c ->
    let s = create n in
    fill s 0 (n - 1) c;
    s


  let blit : t -> int -> t -> int -> int -> unit =
  fun src srcoff dst dstoff len ->
    String.blit src (srcoff * 4) dst (dstoff * 4) (len * 4)


  let iter : (UChar.t -> unit) -> t -> unit =
  fun f s ->
    for i = 0 to length s - 1 do
      f (get s i)
    done


  let iteri : (int -> UChar.t -> unit) -> t -> unit =
  fun f s ->
    for i = 0 to length s - 1 do
      f i (get s i)
    done


  let map : (UChar.t -> UChar.t) -> t -> t =
  fun f s ->
    let mapped = create (length s) in
    for i = 0 to length s - 1 do
      set mapped i (f (get s i))
    done;
    mapped


  let mapi : (int -> UChar.t -> UChar.t) -> t -> t =
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

  let index : t -> UChar.t -> int =
  fun s c ->
    index_rec s (length s) 0 c

  let index_from : t -> int -> UChar.t -> int =
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

  let rindex : t -> UChar.t -> int =
  fun s c ->
    rindex_rec s (length s - 1) c

  let rindex_from : t -> int -> UChar.t -> int =
  fun s i c ->
    if i < -1 || i >= length s then
      invalid_arg "Utf32.rindex_from"
    else
      rindex_rec s i c


  let contains_from : t -> int -> UChar.t -> bool =
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


  let contains : t -> UChar.t -> bool =
  fun s c ->
    contains_from s 0 c

  let rcontains_from : t -> int -> UChar.t -> bool =
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

  let fold_left : 'a. ('a -> UChar.t -> 'a) -> 'a -> t -> 'a =
  fun f x s ->
    fold_left 0 f x s


  let rec fold_lefti i f x s =
    if length s - i > 0 then
      fold_lefti (i + 1) f (f i x (get s i)) s
    else
      x

  let fold_lefti : 'a. (int -> 'a -> UChar.t -> 'a) -> 'a -> t -> 'a =
  fun f x s ->
    fold_lefti 0 f x s


  let rec fold_right i f s x =
    if i >= 0 then
      fold_right (i - 1) f s (f (get s i) x)
    else
      x

  let fold_right : 'a. (UChar.t -> 'a -> 'a) -> t -> 'a -> 'a =
  fun f s x ->
    fold_right (length s - 1) f s x


  let rec fold_righti i f s x =
    if i >= 0 then
      fold_righti (i - 1) f s (f i (get s i) x)
    else
      x

  let fold_righti : 'a. (int -> UChar.t -> 'a -> 'a) -> t -> 'a -> 'a =
  fun f s x ->
    fold_righti (length s - 1) f s x


end


let _ =
  let s = Utf32.make 10 (UChar.chr 0x263a) in
  String.iter (fun c ->
    Printf.printf "%02x " (Char.code c)
  ) s;
  print_endline "--";
  Utf32.iter (fun c ->
    Printf.printf "%x\n" (UChar.code c)
  ) s;
  Utf32.fold_left (fun n c ->
    Printf.printf "%d: %x\n" n (UChar.code c);
    n + 1
  ) 0 s;
  Utf32.fold_lefti (fun i n c ->
    Printf.printf "%d = %d: %x\n" i n (UChar.code c);
    n + 1
  ) 0 s;
  Utf32.fold_right (fun c n ->
    Printf.printf "%d: %x\n" n (UChar.code c);
    n + 1
  ) s 0;
  Utf32.fold_righti (fun i c n ->
    Printf.printf "%d = %d: %x\n" i n (UChar.code c);
    n + 1
  ) s 0;
;;
