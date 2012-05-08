open Codepoint

let string_of_general_category = function
  | Cc -> "Cc"
  | Cf -> "Cf"
  | Co -> "Co"
  | Cs -> "Cs"
  | Ll -> "Ll"
  | Lm -> "Lm"
  | Lo -> "Lo"
  | Lt -> "Lt"
  | Lu -> "Lu"
  | Mc -> "Mc"
  | Me -> "Me"
  | Mn -> "Mn"
  | Nd -> "Nd"
  | Nl -> "Nl"
  | No -> "No"
  | Pc -> "Pc"
  | Pd -> "Pd"
  | Pe -> "Pe"
  | Pf -> "Pf"
  | Pi -> "Pi"
  | Po -> "Po"
  | Ps -> "Ps"
  | Sc -> "Sc"
  | Sk -> "Sk"
  | Sm -> "Sm"
  | So -> "So"
  | Zl -> "Zl"
  | Zp -> "Zp"
  | Zs -> "Zs"


let string_of_bidirectional_category = function
  | AL -> "AL"
  | AN -> "AN"
  | B -> "B"
  | BN -> "BN"
  | CS -> "CS"
  | EN -> "EN"
  | ES -> "ES"
  | ET -> "ET"
  | L -> "L"
  | LRE -> "LRE"
  | LRO -> "LRO"
  | NSM -> "NSM"
  | ON -> "ON"
  | PDF -> "PDF"
  | R -> "R"
  | RLE -> "RLE"
  | RLO -> "RLO"
  | S -> "S"
  | WS -> "WS"


let rec string_of_int_list = function
  | [] -> ""
  | [hd] -> Printf.sprintf "0x%04x" hd
  | hd :: tl -> Printf.sprintf "0x%04x;%s" hd (string_of_int_list tl)

let string_of_character_decomposition_mapping = function
  | Standard, a -> "Standard, [" ^ string_of_int_list a ^ "]"
  | Font, a -> "Font, [" ^ string_of_int_list a ^ "]"
  | NoBreak, a -> "NoBreak, [" ^ string_of_int_list a ^ "]"
  | Initial, a -> "Initial, [" ^ string_of_int_list a ^ "]"
  | Medial, a -> "Medial, [" ^ string_of_int_list a ^ "]"
  | Final, a -> "Final, [" ^ string_of_int_list a ^ "]"
  | Isolated, a -> "Isolated, [" ^ string_of_int_list a ^ "]"
  | Circle, a -> "Circle, [" ^ string_of_int_list a ^ "]"
  | Super, a -> "Super, [" ^ string_of_int_list a ^ "]"
  | Sub, a -> "Sub, [" ^ string_of_int_list a ^ "]"
  | Vertical, a -> "Vertical, [" ^ string_of_int_list a ^ "]"
  | Wide, a -> "Wide, [" ^ string_of_int_list a ^ "]"
  | Narrow, a -> "Narrow, [" ^ string_of_int_list a ^ "]"
  | Small, a -> "Small, [" ^ string_of_int_list a ^ "]"
  | Square, a -> "Square, [" ^ string_of_int_list a ^ "]"
  | Fraction, a -> "Fraction, [" ^ string_of_int_list a ^ "]"
  | Compat, a -> "Compat, [" ^ string_of_int_list a ^ "]"


let string_of_codepoint =
  let opts = function
    | None -> "None"
    | Some x -> "Some (\"" ^ (String.escaped x) ^ "\")"
  in

  let opti = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some (0x%04x)" x
  in

  let optf f = function
    | None -> "None"
    | Some x -> "Some (" ^ f x ^ ")"
  in

  function
  {
    codepoint = codepoint;
    character_name = character_name;
    general_category = general_category;
    canonical_combining_classes = canonical_combining_classes;
    bidirectional_category = bidirectional_category;
    character_decomposition_mapping = character_decomposition_mapping;
    decimal_digit_value = decimal_digit_value;
    digit_value = digit_value;
    numeric_value = numeric_value;
    mirrored = mirrored;
    unicode_1_0_name = unicode_1_0_name;
    comment = comment;
    uppercase_mapping = uppercase_mapping;
    lowercase_mapping = lowercase_mapping;
    titlecase_mapping = titlecase_mapping;
  } ->
    Printf.sprintf "{ codepoint = 0x%04x; character_name = %s; general_category = %s; canonical_combining_classes = %d; bidirectional_category = %s; character_decomposition_mapping = %s; decimal_digit_value = %s; digit_value = %s; numeric_value = %s; mirrored = %s; unicode_1_0_name = %s; comment = %s; uppercase_mapping = %s; lowercase_mapping = %s; titlecase_mapping = %s }"
      codepoint
      (opts character_name)
      (string_of_general_category general_category)
      canonical_combining_classes
      (string_of_bidirectional_category bidirectional_category)
      (optf string_of_character_decomposition_mapping character_decomposition_mapping)
      (opts decimal_digit_value)
      (opts digit_value)
      (opts numeric_value)
      (string_of_bool mirrored)
      (opts unicode_1_0_name)
      (opts comment)
      (opti uppercase_mapping)
      (opti lowercase_mapping)
      (opti titlecase_mapping)


let csv_of_codepoint =
  let opts = function
    | None -> ""
    | Some x -> x
  in

  let opti = function
    | None -> ""
    | Some x -> Printf.sprintf "%04x" x
  in

  let optf f = function
    | None -> ""
    | Some x -> f x
  in
  
  function
  {
    codepoint = codepoint;
    character_name = character_name;
    general_category = general_category;
    canonical_combining_classes = canonical_combining_classes;
    bidirectional_category = bidirectional_category;
    character_decomposition_mapping = character_decomposition_mapping;
    decimal_digit_value = decimal_digit_value;
    digit_value = digit_value;
    numeric_value = numeric_value;
    mirrored = mirrored;
    unicode_1_0_name = unicode_1_0_name;
    comment = comment;
    uppercase_mapping = uppercase_mapping;
    lowercase_mapping = lowercase_mapping;
    titlecase_mapping = titlecase_mapping;
  } ->
    Printf.sprintf "%04x;%s;%s;%d;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s"
      codepoint
      (opts character_name)
      (string_of_general_category general_category)
      canonical_combining_classes
      (string_of_bidirectional_category bidirectional_category)
      (optf string_of_character_decomposition_mapping character_decomposition_mapping)
      (opts decimal_digit_value)
      (opts digit_value)
      (opts numeric_value)
      (string_of_bool mirrored)
      (opts unicode_1_0_name)
      (opts comment)
      (opti uppercase_mapping)
      (opti lowercase_mapping)
      (opti titlecase_mapping)
