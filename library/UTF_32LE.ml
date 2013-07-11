open Util


(*************************************************
 * :: UTF-32LE
 *************************************************)

module Scheme : Encoding.Scheme = struct

  let name = "UTF-32LE"

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


module Encoding = UTF.Make(Scheme)
