let (|>) = BatPervasives.(|>)


let uniescape s =
  let buf = Buffer.create (String.length s * 4) in
  String.iter (fun c ->
    Printf.bprintf buf "\\x%02x" (int_of_char c)
  ) s;
  Buffer.contents buf


let show func result =
  print_string func;
  print_endline result


let test () =
  let open Unicode in
  let open Codepoint in
  let s = "hey 你好ä𤭢 ho" in
  assert ((utf8s_of_utf32s (utf32s_of_utf8s s)) = s);

  let utf16 = Utf16_2 (0xd800, 0xdc00) in
  let utf32 = utf32_of_utf16 utf16 in
  print_endline (string_of_utf16 utf16 ^ " => " ^ string_of_utf32 utf32);
  assert (utf32 = 0x10000);

  Array.iter (fun c ->
    if c.code < 0xd800 || c.code > 0xf8ff then (
      let utf32 = c.code in

      Printf.printf "code: %04x\n" utf32;

      (* from utf32 *)
      let utf8   =   utf8_of_utf32  utf32  in string_of_utf8 utf8 |> show "  utf8_of_utf32  ";
      assert (utf32_of_utf8 utf8 = c.code);
      let utf8s  =  utf8s_of_utf32  utf32  in string_of_utf8s utf8s |> show " utf8s_of_utf32  ";
      assert (utf32_of_utf8s utf8s = c.code);

      let utf16  =  utf16_of_utf32  utf32  in string_of_utf16 utf16 |> show " utf16_of_utf32  ";
      assert (utf32_of_utf16 utf16 = c.code);
      let utf16s = utf16s_of_utf32  utf32  in string_of_utf16s utf16s |> show "utf16s_of_utf32  ";
      assert (utf32_of_utf16s utf16s = c.code);

      (*let utf32  =  utf32_of_utf32  utf32  in string_of_utf32 utf32 |> show " utf32_of_utf32  ";*)
      (*assert (utf32_of_utf32 utf32 = c.code);*)
      let utf32s = utf32s_of_utf32  utf32  in string_of_utf32s utf32s |> show "utf32s_of_utf32  ";
      assert (utf32_of_utf32s utf32s = c.code);

      (* from utf32s *)
      let utf8   =   utf8_of_utf32s utf32s in string_of_utf8 utf8 |> show "  utf8_of_utf32s ";
      assert (utf32_of_utf8 utf8 = c.code);
      let utf8s  =  utf8s_of_utf32s utf32s in string_of_utf8s utf8s |> show " utf8s_of_utf32s ";
      assert (utf32_of_utf8s utf8s = c.code);

      let utf16  =  utf16_of_utf32s utf32s in string_of_utf16 utf16 |> show " utf16_of_utf32s ";
      assert (utf32_of_utf16 utf16 = c.code);
      let utf16s = utf16s_of_utf32s utf32s in string_of_utf16s utf16s |> show "utf16s_of_utf32s ";
      assert (utf32_of_utf16s utf16s = c.code);

      let utf32  =  utf32_of_utf32s utf32s in string_of_utf32 utf32 |> show " utf32_of_utf32s ";
      (*assert (utf32_of_utf32 utf32 = c.code);*)
      (*let utf32s = utf32s_of_utf32utf3utf322s utf32s in string_of_utf32s utf32s |> show "utf32s_of_utf32s ";*)
      (*assert (utf32_of_utf32s utf32s = c.code);*)

      (* from utf16 *)
      let utf8   =   utf8_of_utf16  utf16  in string_of_utf8 utf8 |> show "  utf8_of_utf16  ";
      assert (utf32_of_utf8 utf8 = c.code);
      let utf8s  =  utf8s_of_utf16  utf16  in string_of_utf8s utf8s |> show " utf8s_of_utf16  ";
      assert (utf32_of_utf8s utf8s = c.code);

      (*let utf16  =  utf16_of_utf16  utf16  in string_of_utf16 utf16 |> show " utf16_of_utf16  ";*)
      (*assert (utf32_of_utf16 utf16 = c.code);*)
      let utf16s = utf16s_of_utf16  utf16  in string_of_utf16s utf16s |> show "utf16s_of_utf16  ";
      assert (utf32_of_utf16s utf16s = c.code);

      let utf32  =  utf32_of_utf16  utf16  in string_of_utf32 utf32 |> show " utf32_of_utf16  ";
      (*assert (utf32_of_utf32 utf32 = c.code);*)
      let utf32s = utf32s_of_utf16  utf16  in string_of_utf32s utf32s |> show "utf32s_of_utf16  ";
      assert (utf32_of_utf32s utf32s = c.code);

      (* from utf16s *)
      let utf8   =   utf8_of_utf16s utf16s in string_of_utf8 utf8 |> show "  utf8_of_utf16s ";
      assert (utf32_of_utf8 utf8 = c.code);
      let utf8s  =  utf8s_of_utf16s utf16s in string_of_utf8s utf8s |> show " utf8s_of_utf16s ";
      assert (utf32_of_utf8s utf8s = c.code);

      let utf16  =  utf16_of_utf16s utf16s in string_of_utf16 utf16 |> show " utf16_of_utf16s ";
      assert (utf32_of_utf16 utf16 = c.code);
      (*let utf16s = utf16s_of_utf16s utf16s in string_of_utf16s utf16s |> show "utf16s_of_utf16s ";*)
      (*assert (utf32_of_utf16s utf16s = c.code);*)

      let utf32  =  utf32_of_utf16s utf16s in string_of_utf32 utf32 |> show " utf32_of_utf16s ";
      (*assert (utf32_of_utf32 utf32 = c.code);*)
      let utf32s = utf32s_of_utf16s utf16s in string_of_utf32s utf32s |> show "utf32s_of_utf16s ";
      assert (utf32_of_utf32s utf32s = c.code);

      (* from utf8 *)
      (*let utf8   =   utf8_of_utf8   utf8   in string_of_utf8 utf8 |> show "  utf8_of_utf8   ";*)
      (*assert (utf32_of_utf8 utf8 = c.code);*)
      let utf8s  =  utf8s_of_utf8   utf8   in string_of_utf8s utf8s |> show " utf8s_of_utf8   ";
      assert (utf32_of_utf8s utf8s = c.code);

      let utf16  =  utf16_of_utf8   utf8   in string_of_utf16 utf16 |> show " utf16_of_utf8   ";
      assert (utf32_of_utf16 utf16 = c.code);
      let utf16s = utf16s_of_utf8   utf8   in string_of_utf16s utf16s |> show "utf16s_of_utf8   ";
      assert (utf32_of_utf16s utf16s = c.code);

      let utf32  =  utf32_of_utf8   utf8   in string_of_utf32 utf32 |> show " utf32_of_utf8   ";
      (*assert (utf32_of_utf32 utf32 = c.code);*)
      let utf32s = utf32s_of_utf8   utf8   in string_of_utf32s utf32s |> show "utf32s_of_utf8   ";
      assert (utf32_of_utf32s utf32s = c.code);

      (* from utf8s *)
      let utf8   =   utf8_of_utf8s  utf8s  in string_of_utf8 utf8 |> show "  utf8_of_utf8s  ";
      assert (utf32_of_utf8 utf8 = c.code);
      (*let utf8s  =  utf8s_of_utf8s  utf8s  in string_of_utf8s utf8s |> show " utf8s_of_utf8s  ";*)
      (*assert (utf32_of_utf8s utf8s = c.code);*)

      let utf16  =  utf16_of_utf8s  utf8s  in string_of_utf16 utf16 |> show " utf16_of_utf8s  ";
      assert (utf32_of_utf16 utf16 = c.code);
      let utf16s = utf16s_of_utf8s  utf8s  in string_of_utf16s utf16s |> show "utf16s_of_utf8s  ";
      assert (utf32_of_utf16s utf16s = c.code);

      let utf32  =  utf32_of_utf8s  utf8s  in string_of_utf32 utf32 |> show " utf32_of_utf8s  ";
      (*assert (utf32_of_utf32 utf32 = c.code);*)
      let utf32s = utf32s_of_utf8s  utf8s  in string_of_utf32s utf32s |> show "utf32s_of_utf8s  ";
      assert (utf32_of_utf32s utf32s = c.code);

      assert (utf32 = c.code);
    )

  ) Udb_data.data;
;;
