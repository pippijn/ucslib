let dump = Make_udb.dump stdout
let csv = Make_udb.csv stdout

let uniescape s =
  let buf = Buffer.create (String.length s * 4) in
  String.iter (fun c ->
    Printf.bprintf buf "\\x%02x" (int_of_char c)
  ) s;
  Buffer.contents buf


let () =
  let open Codepoint in
  dump (Udb.by_codepoint 0x263a);
  (*csv (Udb.by_general_category Codepoint.Lm);*)
  let s = "hey 你好ä𤭢 ho" in
  assert ((Unicode.utf8_of_utf32 (Unicode.utf32_of_utf8 s)) = s);

  Array.iter (fun c ->

    let utf8 = Unicode.utf8_of_utf32 [c.codepoint] in
    let utf32 = Unicode.utf32_of_utf8 utf8 in

    if utf32 <> [c.codepoint] then
      let s32 = Udb_print.string_of_int_list utf32 in
      let s8 = uniescape utf8 in
      let msg = Printf.sprintf "code point %04x is not surjective (got [%s] for \"%s\")" c.codepoint s32 s8 in
      failwith msg

  ) Udb_data.data;
