let (|>) = BatPervasives.(|>)

(*************************************************
 * :: Types
 *************************************************)

type utf8   =
  | Utf8_1 of char
  | Utf8_2 of char * char
  | Utf8_3 of char * char * char
  | Utf8_4 of char * char * char * char
  | Utf8_5 of char * char * char * char * char
  | Utf8_6 of char * char * char * char * char * char

type utf16  =
  | Utf16_1 of int
  | Utf16_2 of int * int

type utf32  = int

type utf8s  = string
type utf16s = int list
type utf32s = int list


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

let mask16 = (land) 0xffff

let is_lead_surrogate code =
  code >= lead_surrogate_min && code <= lead_surrogate_max

let is_trail_surrogate code =
  code >= trail_surrogate_min && code <= trail_surrogate_max

let is_surrogate code =
  code >= lead_surrogate_min && code <= trail_surrogate_max



(*************************************************
 * :: Convert between single character and string
 *************************************************)

let utf8_of_utf8s : utf8s -> utf8 = fun s ->
  match String.length s with
  | 1 -> Utf8_1 (s.[0])
  | 2 -> Utf8_2 (s.[0], s.[1])
  | 3 -> Utf8_3 (s.[0], s.[1], s.[2])
  | 4 -> Utf8_4 (s.[0], s.[1], s.[2], s.[3])
  | 5 -> Utf8_5 (s.[0], s.[1], s.[2], s.[3], s.[4])
  | 6 -> Utf8_6 (s.[0], s.[1], s.[2], s.[3], s.[4], s.[5])
  | _ -> failwith "utf8_of_utf8s"

let list_of_utf8 : utf8 -> char list = function
  | Utf8_1 (s0) -> [s0]
  | Utf8_2 (s0, s1) -> [s0; s1]
  | Utf8_3 (s0, s1, s2) -> [s0; s1; s2]
  | Utf8_4 (s0, s1, s2, s3) -> [s0; s1; s2; s3]
  | Utf8_5 (s0, s1, s2, s3, s4) -> [s0; s1; s2; s3; s4]
  | Utf8_6 (s0, s1, s2, s3, s4, s5) -> [s0; s1; s2; s3; s4; s5]

let utf8_of_list : char list -> utf8 = function
  | [s0] -> Utf8_1 (s0)
  | [s0; s1] -> Utf8_2 (s0, s1)
  | [s0; s1; s2] -> Utf8_3 (s0, s1, s2)
  | [s0; s1; s2; s3] -> Utf8_4 (s0, s1, s2, s3)
  | [s0; s1; s2; s3; s4] -> Utf8_5 (s0, s1, s2, s3, s4)
  | [s0; s1; s2; s3; s4; s5] -> Utf8_6 (s0, s1, s2, s3, s4, s5)
  | _ -> failwith "utf8_of_list"

let utf8s_of_utf8 : utf8 -> utf8s = fun utf8 ->
  BatString.of_list (list_of_utf8 utf8)


let utf16_of_utf16s : utf16s -> utf16 = function
  | [cp] ->
      assert (not (is_surrogate cp));
      Utf16_1 (cp)
  | [lead; trail] ->
      assert (is_lead_surrogate lead);
      assert (is_trail_surrogate trail);
      Utf16_2 (lead, trail)
  | _ ->
      failwith "utf16_of_utf16s"

let utf16s_of_utf16 : utf16 -> utf16s = function
  | Utf16_1 (cp) ->
      assert (not (is_surrogate cp));
      [cp]
  | Utf16_2 (lead, trail) ->
      assert (is_lead_surrogate lead);
      assert (is_trail_surrogate trail);
      [lead; trail]


let utf32_of_utf32s : utf32s -> utf32 = function
  | [cp] -> cp
  | _ -> failwith "utf32_of_utf32s"


(*************************************************
 * :: UTF-32 -> UTF-8
 *************************************************)

let utf8_of_utf32 : utf32 -> utf8 = fun chr ->
  let continuation n = ((chr lsr (6 * n)) land 0x3f) lor 0x80 in
  let leading n mask =  (chr lsr (6 * n))            lor mask in

  if chr <= 0x007f then (* one octet *)
    Utf8_1 (
      char_of_int chr
    )
  else if chr <= 0x07ff then (* two octets *)
    Utf8_2 (
      char_of_int (leading 1 0xc0),
      char_of_int (continuation 0)
    )
  else if chr <= 0xffff then (* three octets *)
    Utf8_3 (
      char_of_int (leading 2 0xe0),
      char_of_int (continuation 1),
      char_of_int (continuation 0)
    )
  else if chr <= 0x1fffff then (* four octets *)
    Utf8_4 (
      char_of_int (leading 3 0xf0),
      char_of_int (continuation 2),
      char_of_int (continuation 1),
      char_of_int (continuation 0)
    )
  else if chr <= 0x3ffffff then (* five octets *)
    Utf8_5 (
      char_of_int (leading 4 0xf8),
      char_of_int (continuation 3),
      char_of_int (continuation 2),
      char_of_int (continuation 1),
      char_of_int (continuation 0)
    )
  else if chr <= 0x7fffffff then (* six octets *)
    Utf8_6 (
      char_of_int (leading 5 0xfc),
      char_of_int (continuation 4),
      char_of_int (continuation 3),
      char_of_int (continuation 2),
      char_of_int (continuation 1),
      char_of_int (continuation 0)
    )
  else
    raise (Invalid_argument "utf8_char")


(*************************************************
 * :: UTF-16 -> UTF-32
 *************************************************)

let utf32_of_utf16 : utf16 -> utf32 = function
  | Utf16_1 (cp) ->
      assert (not (is_surrogate cp));
      cp
  | Utf16_2 (lead, trail) ->
      assert (is_lead_surrogate lead);
      assert (is_trail_surrogate trail);
      ((lead - 0xd800) * 0x400) + (trail - 0xdc00) + 0x10000


(*************************************************
 * :: UTF-16 -> UTF-8
 *************************************************)

let utf8_of_utf16 : utf16 -> utf8 = fun utf16 ->
  utf8_of_utf32 (utf32_of_utf16 utf16)


(*************************************************
 * :: UTF-8 -> UTF-32
 *************************************************)

let utf32_of_utf8 : utf8 -> utf32 = function
  | Utf8_1 (c1) ->
      ((Char.code c1 lsl (6 * 0)) land 0x000000ff)
  | Utf8_2 (c1, c2) ->
      ((Char.code c2 lsl (6 * 0)) land 0x0000003f) +
      ((Char.code c1 lsl (6 * 1)) land 0x000007ff)
  | Utf8_3 (c1, c2, c3) ->
      ((Char.code c3 lsl (6 * 0)) land 0x0000003f) +
      ((Char.code c2 lsl (6 * 1)) land 0x00000fff) +
      ((Char.code c1 lsl (6 * 2)) land 0x0000ffff)
  | Utf8_4 (c1, c2, c3, c4) ->
      ((Char.code c4 lsl (6 * 0)) land 0x0000003f) +
      ((Char.code c3 lsl (6 * 1)) land 0x00000fff) +
      ((Char.code c2 lsl (6 * 2)) land 0x0003ffff) +
      ((Char.code c1 lsl (6 * 3)) land 0x001fffff)
  (* FIXME: I don't know whether these are correct (probably not). *)
  | Utf8_5 (c1, c2, c3, c4, c5) ->
      ((Char.code c5 lsl (6 * 0)) land 0x0000003f) +
      ((Char.code c4 lsl (6 * 1)) land 0x00000fff) +
      ((Char.code c3 lsl (6 * 2)) land 0x0000ffff) +
      ((Char.code c2 lsl (6 * 3)) land 0x001fffff) +
      ((Char.code c1 lsl (6 * 4)) land 0x03ffffff)
  | Utf8_6 (c1, c2, c3, c4, c5, c6) ->
      ((Char.code c6 lsl (6 * 0)) land 0x0000003f) +
      ((Char.code c5 lsl (6 * 1)) land 0x00000fff) +
      ((Char.code c4 lsl (6 * 2)) land 0x0000ffff) +
      ((Char.code c3 lsl (6 * 3)) land 0x001fffff) +
      ((Char.code c2 lsl (6 * 4)) land 0x03ffffff) +
      ((Char.code c1 lsl (6 * 5)) land 0x7fffffff)


(*************************************************
 * :: UTF-32 -> UTF-16
 *************************************************)

let utf16_of_utf32 : utf32 -> utf16 = fun cp ->
  if cp <= 0xffff then
    Utf16_1 (cp)
  else
    Utf16_2 (
      mask16 ((cp lsr 10) + lead_offset),
      mask16 ((cp land 0x3ff) + trail_surrogate_min)
    )


(*************************************************
 * :: UTF-8 -> UTF-16
 *************************************************)

let utf16_of_utf8 : utf8 -> utf16 = fun utf8 ->
  utf16_of_utf32 (utf32_of_utf8 utf8)


(*************************************************
 * :: UTF-8 string -> UTF-32 string
 *************************************************)

type utf8_state =
  | Initial
  | Continuation of int * char list


(* The number of trailing octets *)
let utf8_length c =
  let d = Char.code c in
  if d >= 0x00 && d < 0x80 then 0 else
  if d >= 0xc0 && d < 0xe0 then 1 else
  if d >= 0xe0 && d < 0xf0 then 2 else
  if d >= 0xf0 && d < 0xf8 then 3 else
  if d >= 0xf8 && d < 0xfc then 4 else
  if d >= 0xfc && d < 0xfe then 5 else
  failwith "utf8_length"


let utf32_of_utf8_rev chars =
  utf32_of_utf8 (utf8_of_list (List.rev chars))


let rec next_utf32 (state, cps) c =
  match state with
  | Initial when utf8_length c = 0 ->
      Initial, Char.code c :: cps
  | Initial ->
      Continuation (utf8_length c, [c]), cps

  | Continuation (0, chars) ->
      next_utf32 (Initial, utf32_of_utf8_rev chars :: cps) c
  | Continuation (n, chars) ->
      Continuation (n - 1, c :: chars), cps


let utf32s_of_utf8s s =
  let utf32 =
    match BatString.fold_left next_utf32 (Initial, []) s with
    | Initial, utf32 ->
        utf32
    | Continuation (0, chars), utf32 ->
        utf32_of_utf8_rev chars :: utf32
    | _ -> failwith "utf32_of_utf8"
  in

  List.rev utf32


(*************************************************
 * :: UTF-8 string -> UTF-16 string
 *************************************************)

let utf16s_rev_of_charlist_rev chars =
  match utf16_of_utf8 (utf8_of_list (List.rev chars)) with
  | Utf16_1 (cp) -> [cp]
  | Utf16_2 (lead, trail) -> [trail; lead]


let rec next_utf16 : (utf8_state * utf16s) -> char -> (utf8_state * utf16s) = fun (state, cps) c ->
  match state with
  | Initial when utf8_length c = 0 ->
      Initial, Char.code c :: cps
  | Initial ->
      Continuation (utf8_length c, [c]), cps

  | Continuation (0, chars) ->
      next_utf32 (Initial, utf16s_rev_of_charlist_rev chars @ cps) c
  | Continuation (n, chars) ->
      Continuation (n - 1, c :: chars), cps


let utf16s_of_utf8s : utf8s -> utf16s = fun s ->
  let utf16 =
    match BatString.fold_left next_utf16 (Initial, []) s with
    | Initial, utf16 ->
        utf16
    | Continuation (0, chars), utf16 ->
        utf16s_rev_of_charlist_rev chars @ utf16
    | _ -> failwith "utf16_of_utf8"
  in

  List.rev utf16


let utf8s_of_utf32 : utf32 -> utf8s = fun c ->
  utf8s_of_utf8 (utf8_of_utf32 c)

let utf8s_of_utf16 : utf16 -> utf8s = fun utf16 ->
  utf8s_of_utf32 (utf32_of_utf16 utf16)


(*************************************************
 * :: UTF-32 string -> UTF-8 string
 *************************************************)

let utf8s_of_utf32s : utf32s -> utf8s = fun s ->
  let buf = Buffer.create (List.length s) in
  List.iter (fun c ->
    Buffer.add_string buf (utf8s_of_utf32 c)
  ) s;
  Buffer.contents buf



(*************************************************
 * :: Convert between single character and string
 *************************************************)

let utf32_of_utf16s : utf16s -> utf32 = fun utf16s ->
  utf32_of_utf16 (utf16_of_utf16s utf16s)

let utf16_of_utf8s : utf8s -> utf16 = fun utf8s ->
  utf16_of_utf8 (utf8_of_utf8s utf8s)


let utf32_of_utf8s : utf8s -> utf32 = fun utf8s ->
  utf32_of_utf8 (utf8_of_utf8s utf8s)

let utf32s_of_utf32 : utf32 -> utf32s = fun cp ->
  [cp]

let utf32s_of_utf8 cp =
  utf32s_of_utf8s (utf8s_of_utf8 cp)

let utf16s_of_utf32 cp =
  utf16s_of_utf16 (utf16_of_utf32 cp)

let utf32s_of_utf16s : utf16s -> utf32s = fun s ->
  let next_utf32 (lead, utf16s) c =
    match lead with
    | None ->
        assert (not (is_trail_surrogate c));
        if is_lead_surrogate c then
          (Some c, utf16s)
        else
          (None, c :: utf16s)

    | Some lead ->
        assert (is_trail_surrogate c);
        (None, utf32_of_utf16 (Utf16_2 (lead, c)) :: utf16s)
  in

  List.fold_left next_utf32 (None, []) s
  |> snd
  |> List.rev

let utf8s_of_utf16s : utf16s -> utf8s = fun s ->
  utf8s_of_utf32s (utf32s_of_utf16s s)

let utf32s_of_utf16 cp =
  utf32s_of_utf32 (utf32_of_utf16 cp)

let utf8_of_utf16s s =
  utf8_of_utf16 (utf16_of_utf16s s)

let utf16s_of_utf32s s =
  List.fold_left (fun utf16s utf32 ->
    utf16s_of_utf32 utf32 @ utf16s
  ) [] s

let utf16_of_utf32s s =
  utf16_of_utf32 (utf32_of_utf32s s)

let utf8_of_utf32s s =
  utf8_of_utf32 (utf32_of_utf32s s)

let utf16s_of_utf8 utf8 =
  utf16s_of_utf32 (utf32_of_utf8 utf8)


let string_of_utf8 = function
  | Utf8_1 (s0) -> Printf.sprintf "[%02x]" (Char.code s0)
  | Utf8_2 (s0, s1) -> Printf.sprintf "[%02x; %02x]" (Char.code s0) (Char.code s1)
  | Utf8_3 (s0, s1, s2) -> Printf.sprintf "[%02x; %02x; %02x]" (Char.code s0) (Char.code s1) (Char.code s2)
  | Utf8_4 (s0, s1, s2, s3) -> Printf.sprintf "[%02x; %02x; %02x; %02x]" (Char.code s0) (Char.code s1) (Char.code s2) (Char.code s3)
  | Utf8_5 (s0, s1, s2, s3, s4) -> Printf.sprintf "[%02x; %02x; %02x; %02x; %02x]" (Char.code s0) (Char.code s1) (Char.code s2) (Char.code s3) (Char.code s4)
  | Utf8_6 (s0, s1, s2, s3, s4, s5) -> Printf.sprintf "[%02x; %02x; %02x; %02x; %02x; %02x]" (Char.code s0) (Char.code s1) (Char.code s2) (Char.code s3) (Char.code s4) (Char.code s5)

let string_of_utf8s s =
  let inner =
    BatString.to_list s
    |> List.map Char.code
    |> List.map (Printf.sprintf "%02x")
    |> String.concat "; "
  in
  "[" ^ inner ^ "]"

let string_of_utf16 = function
  | Utf16_1 (s0) -> Printf.sprintf "[%04x]" s0
  | Utf16_2 (s0, s1) -> Printf.sprintf "[%04x; %04x]" s0 s1

let string_of_utf16s s =
  let inner =
    List.map (Printf.sprintf "%04x") s
    |> String.concat "; "
  in
  "[" ^ inner ^ "]"

let string_of_utf32 cp =
  Printf.sprintf "[%04x]" cp

let string_of_utf32s s =
  let inner =
    List.map (Printf.sprintf "%04x") s
    |> String.concat "; "
  in
  "[" ^ inner ^ "]"
