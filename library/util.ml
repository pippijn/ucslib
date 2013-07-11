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
