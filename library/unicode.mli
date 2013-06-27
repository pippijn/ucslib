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


(* Single UTF-8 of single UTF-16/32 *)
val utf8_of_utf16 : utf16 -> utf8
val utf8_of_utf32 : utf32 -> utf8

(* Single UTF-8 of UTF-8/16/32 string containing one character *)
val utf8_of_utf8s : utf8s -> utf8
val utf8_of_utf16s : utf16s -> utf8
val utf8_of_utf32s : utf32s -> utf8

(* UTF-8 string of single UTF-8/16/32 *)
val utf8s_of_utf8 : utf8 -> utf8s
val utf8s_of_utf16 : utf16 -> utf8s
val utf8s_of_utf32 : utf32 -> utf8s

(* UTF-8 string of UTF-16/32 string containing one character *)
val utf8s_of_utf16s : utf16s -> utf8s
val utf8s_of_utf32s : utf32s -> utf8s


(* Single UTF-16 of single UTF-8/32 *)
val utf16_of_utf8 : utf8 -> utf16
val utf16_of_utf32 : utf32 -> utf16

(* Single UTF-16 of UTF-8/16/32 string containing one character *)
val utf16_of_utf8s : utf8s -> utf16
val utf16_of_utf16s : utf16s -> utf16
val utf16_of_utf32s : utf32s -> utf16

(* UTF-16 string of single UTF-8/16/32 *)
val utf16s_of_utf8 : utf8 -> utf16s
val utf16s_of_utf16 : utf16 -> utf16s
val utf16s_of_utf32 : utf32 -> utf16s

(* UTF-16 string of UTF-8/32 string containing one character *)
val utf16s_of_utf8s : utf8s -> utf16s
val utf16s_of_utf32s : utf32s -> utf16s


(* Single UTF-32 of single UTF-8/16 *)
val utf32_of_utf8 : utf8 -> utf32
val utf32_of_utf16 : utf16 -> utf32

(* Single UTF-32 of UTF-8/16/32 string containing one character *)
val utf32_of_utf8s : utf8s -> utf32
val utf32_of_utf16s : utf16s -> utf32
val utf32_of_utf32s : utf32s -> utf32

(* UTF-32 string of single UTF-8/16/32 *)
val utf32s_of_utf8 : utf8 -> utf32s
val utf32s_of_utf16 : utf16 -> utf32s
val utf32s_of_utf32 : utf32 -> utf32s

(* UTF-32 string of UTF-8/16 string containing one character *)
val utf32s_of_utf8s : utf8s -> utf32s
val utf32s_of_utf16s : utf16s -> utf32s


(* Show their string representation *)
val string_of_utf8   : utf8   -> string
val string_of_utf8s  : utf8s  -> string
val string_of_utf16  : utf16  -> string
val string_of_utf16s : utf16s -> string
val string_of_utf32  : utf32  -> string
val string_of_utf32s : utf32s -> string
