open Util


(*************************************************
 * :: UTF-32BE
 *************************************************)

module Scheme : Encoding.Scheme = struct

  let name = "UTF-32BE"

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


module Encoding = UTF.Make(Scheme)
