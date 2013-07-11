open Util


(*************************************************
 * :: UTF-16BE
 *************************************************)

module Scheme : Encoding.Scheme = struct

  let name = "UTF-16BE"

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


module Encoding = UTF.Make(Scheme)
