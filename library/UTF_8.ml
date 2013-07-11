open Util

(*************************************************
 * :: UTF-8
 *************************************************)

module Scheme : Encoding.Scheme = struct

  let name = "UTF-8"

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


module Encoding = UTF.Make(Scheme)
