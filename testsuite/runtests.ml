open_out "testsuite.rst";;

open Ucslib


(*************************************************
 * :: Unit tests.
 *************************************************)

let utf_test utf =
  let module U = (val utf : Encoding.S) in
  let open U in

  Printf.printf "test encoding: %s\n" U.name;

  let buf = Buffer.create 4 in
  Buffer.add_char buf (UChar.chr 0x263a);
  Buffer.add_char buf (UChar.chr 0x1263a);
  Buffer.add_char buf (UChar.chr 0x263a);
  Buffer.add_char buf (UChar.chr 0x263a);

  let s = Buffer.contents buf in

  ByteString.iter (fun c ->
    Printf.printf "%02x " (Char.code c)
  ) (s :> string);
  print_endline "--";

  print_endline "fold_left";
  ignore (String.fold_left (fun n cp ->
    Printf.printf "%d: %x\n" n (UChar.code cp);
    n + 1
  ) 0 s);

  print_endline "map";
  let s =
    String.map (fun cp ->
      UChar.chr (UChar.code cp - 1)
    ) s
  in

  ignore (String.fold_left (fun n cp ->
    Printf.printf "%d: %x\n" n (UChar.code cp);
    n + 1
  ) 0 s);

  ()


let _ =
  match Sys.argv with
  | [|_; "-depend"|] -> ()
  | _ ->
      let s = UTF_8.Encoding.String.adopt "hel\xe2\x98\xba" in
      ignore (UTF_8.Encoding.String.fold_left (fun n cp ->
        Printf.printf "%d: %x\n" n (UChar.code cp);
        n + 1
      ) 0 s);

      List.iter utf_test [
        (module UTF_8.Encoding : Encoding.S);
        (module UTF_16BE.Encoding : Encoding.S);
        (module UTF_16LE.Encoding : Encoding.S);
        (module UTF_32BE.Encoding : Encoding.S);
        (module UTF_32LE.Encoding : Encoding.S);
      ]
