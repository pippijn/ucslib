{
  open Codepoint

  let empty = {
    codepoint = 0;
    character_name = None;
    general_category = Cc;
    canonical_combining_classes = 0;
    bidirectional_category = L;
    character_decomposition_mapping = None;
    decimal_digit_value = None;
    digit_value = None;
    numeric_value = None;
    mirrored = false;
    unicode_1_0_name = None;
    comment = None;
    uppercase_mapping = None;
    lowercase_mapping = None;
    titlecase_mapping = None;
  }

  let int_of_digit = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'A' -> 10
    | 'B' -> 11
    | 'C' -> 12
    | 'D' -> 13
    | 'E' -> 14
    | 'F' -> 15
    | _ -> raise ExPervasives.Impossible

  let int_of_hex_string s =
    fst (ExString.fold_right (fun c (value, multiplier) ->
      value + (int_of_digit c) * multiplier, multiplier * 16
    ) s (0, 1))

  let opt f = function
    | None -> None
    | Some x -> Some (f x)
}


let d = ['0'-'9']
let x = ['0'-'9' 'A'-'F']
let s = ' '+
let field = [^';''\n']+


rule codepoint cp c = parse
  | (x+ as d) ';'		{ character_name (c :: cp) { empty with codepoint = int_of_hex_string d } lexbuf }

  | _ as c			{ failwith ("codepoint: " ^ Char.escaped c) }

  | eof				{ c :: cp }


and character_name cp c = parse
  | (field as f)? ';'		{ general_category cp { c with character_name = f } lexbuf }

  | _ as c			{ failwith ("character_name: " ^ Char.escaped c) }


and general_category cp c = parse
  | "Cc" ';'			{ canonical_combining_classes cp { c with general_category = Cc } lexbuf }
  | "Cf" ';'			{ canonical_combining_classes cp { c with general_category = Cf } lexbuf }
  | "Co" ';'			{ canonical_combining_classes cp { c with general_category = Co } lexbuf }
  | "Cs" ';'			{ canonical_combining_classes cp { c with general_category = Cs } lexbuf }
  | "Ll" ';'			{ canonical_combining_classes cp { c with general_category = Ll } lexbuf }
  | "Lm" ';'			{ canonical_combining_classes cp { c with general_category = Lm } lexbuf }
  | "Lo" ';'			{ canonical_combining_classes cp { c with general_category = Lo } lexbuf }
  | "Lt" ';'			{ canonical_combining_classes cp { c with general_category = Lt } lexbuf }
  | "Lu" ';'			{ canonical_combining_classes cp { c with general_category = Lu } lexbuf }
  | "Mc" ';'			{ canonical_combining_classes cp { c with general_category = Mc } lexbuf }
  | "Me" ';'			{ canonical_combining_classes cp { c with general_category = Me } lexbuf }
  | "Mn" ';'			{ canonical_combining_classes cp { c with general_category = Mn } lexbuf }
  | "Nd" ';'			{ canonical_combining_classes cp { c with general_category = Nd } lexbuf }
  | "Nl" ';'			{ canonical_combining_classes cp { c with general_category = Nl } lexbuf }
  | "No" ';'			{ canonical_combining_classes cp { c with general_category = No } lexbuf }
  | "Pc" ';'			{ canonical_combining_classes cp { c with general_category = Pc } lexbuf }
  | "Pd" ';'			{ canonical_combining_classes cp { c with general_category = Pd } lexbuf }
  | "Pe" ';'			{ canonical_combining_classes cp { c with general_category = Pe } lexbuf }
  | "Pf" ';'			{ canonical_combining_classes cp { c with general_category = Pf } lexbuf }
  | "Pi" ';'			{ canonical_combining_classes cp { c with general_category = Pi } lexbuf }
  | "Po" ';'			{ canonical_combining_classes cp { c with general_category = Po } lexbuf }
  | "Ps" ';'			{ canonical_combining_classes cp { c with general_category = Ps } lexbuf }
  | "Sc" ';'			{ canonical_combining_classes cp { c with general_category = Sc } lexbuf }
  | "Sk" ';'			{ canonical_combining_classes cp { c with general_category = Sk } lexbuf }
  | "Sm" ';'			{ canonical_combining_classes cp { c with general_category = Sm } lexbuf }
  | "So" ';'			{ canonical_combining_classes cp { c with general_category = So } lexbuf }
  | "Zl" ';'			{ canonical_combining_classes cp { c with general_category = Zl } lexbuf }
  | "Zp" ';'			{ canonical_combining_classes cp { c with general_category = Zp } lexbuf }
  | "Zs" ';'			{ canonical_combining_classes cp { c with general_category = Zs } lexbuf }

  | field as c			{ failwith ("general_category: " ^ c) }
  | _ as c			{ failwith ("character_decomposition_mapping: " ^ Char.escaped c) }


and canonical_combining_classes cp c = parse
  | d+ as d ';'			{ bidirectional_category cp { c with canonical_combining_classes = int_of_string d } lexbuf }

  | field as c			{ failwith ("canonical_combining_classes: " ^ c) }
  | _ as c			{ failwith ("character_decomposition_mapping: " ^ Char.escaped c) }


and bidirectional_category cp c = parse
  | "AL" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = AL } lexbuf }
  | "AN" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = AN } lexbuf }
  | "B" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = B } lexbuf }
  | "BN" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = BN } lexbuf }
  | "CS" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = CS } lexbuf }
  | "EN" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = EN } lexbuf }
  | "ES" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = ES } lexbuf }
  | "ET" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = ET } lexbuf }
  | "L" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = L } lexbuf }
  | "LRE" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = LRE } lexbuf }
  | "LRO" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = LRO } lexbuf }
  | "NSM" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = NSM } lexbuf }
  | "ON" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = ON } lexbuf }
  | "PDF" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = PDF } lexbuf }
  | "R" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = B } lexbuf }
  | "RLE" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = RLE } lexbuf }
  | "RLO" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = RLO } lexbuf }
  | "S" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = S } lexbuf }
  | "WS" ';'			{ character_decomposition_mapping cp { c with bidirectional_category = WS } lexbuf }

  | field as c			{ failwith ("bidirectional_category: " ^ c) }
  | _ as c			{ failwith ("character_decomposition_mapping: " ^ Char.escaped c) }


and character_decomposition_mapping cp c = parse
  | "<font>"			{ character_decomposition cp c Font [] lexbuf }
  | "<noBreak>"			{ character_decomposition cp c NoBreak [] lexbuf }
  | "<initial>"			{ character_decomposition cp c Initial [] lexbuf }
  | "<medial>"			{ character_decomposition cp c Medial [] lexbuf }
  | "<final>"			{ character_decomposition cp c Final [] lexbuf }
  | "<isolated>"		{ character_decomposition cp c Isolated [] lexbuf }
  | "<circle>"			{ character_decomposition cp c Circle [] lexbuf }
  | "<super>"			{ character_decomposition cp c Super [] lexbuf }
  | "<sub>"			{ character_decomposition cp c Sub [] lexbuf }
  | "<vertical>"		{ character_decomposition cp c Vertical [] lexbuf }
  | "<wide>"			{ character_decomposition cp c Wide [] lexbuf }
  | "<narrow>"			{ character_decomposition cp c Narrow [] lexbuf }
  | "<small>"			{ character_decomposition cp c Small [] lexbuf }
  | "<square>"			{ character_decomposition cp c Square [] lexbuf }
  | "<fraction>"		{ character_decomposition cp c Fraction [] lexbuf }
  | "<compat>"			{ character_decomposition cp c Compat [] lexbuf }
  | (x+ as a)			{ character_decomposition cp c Standard [int_of_hex_string a] lexbuf }
  | ';'				{ decimal_digit_value cp { c with character_decomposition_mapping = None } lexbuf }

  | _ as c			{ failwith ("character_decomposition_mapping: " ^ Char.escaped c) }


and character_decomposition cp c t l = parse
  | (x+ as a)			{ character_decomposition cp c t (int_of_hex_string a :: l) lexbuf }
  | ' '				{ character_decomposition cp c t l lexbuf } 
  | ';'				{ decimal_digit_value cp { c with character_decomposition_mapping = Some (t, List.rev l) } lexbuf }

  | _ as c			{ failwith ("character_decomposition: " ^ Char.escaped c) }


and decimal_digit_value cp c = parse
  | (field as f)? ';'		{ digit_value cp { c with decimal_digit_value = f } lexbuf }

  | _ as c			{ failwith ("decimal_digit_value: " ^ Char.escaped c) }


and digit_value cp c = parse
  | (field as f)? ';'		{ numeric_value cp { c with digit_value = f } lexbuf }

  | _ as c			{ failwith ("digit_value: " ^ Char.escaped c) }


and numeric_value cp c = parse
  | (field as f)? ';'		{ mirrored cp { c with numeric_value = f } lexbuf }

  | _ as c			{ failwith ("numeric_value: " ^ Char.escaped c) }


and mirrored cp c = parse
  | "Y" ';'			{ unicode_1_0_name cp { c with mirrored = true } lexbuf }
  | "N" ';'			{ unicode_1_0_name cp { c with mirrored = false } lexbuf }

  | _ as c			{ failwith ("mirrored: " ^ Char.escaped c) }


and unicode_1_0_name cp c = parse
  | (field as f)? ';'		{ comment cp { c with unicode_1_0_name = f } lexbuf }

  | _ as c			{ failwith ("unicode_1_0_name: " ^ Char.escaped c) }


and comment cp c = parse
  | (field as f)? ';'		{ uppercase_mapping cp { c with comment = f } lexbuf }

  | _ as c			{ failwith ("comment: " ^ Char.escaped c) }


and uppercase_mapping cp c = parse
  | (field as f)? ';'		{ lowercase_mapping cp { c with uppercase_mapping = opt int_of_hex_string f } lexbuf }

  | _ as c			{ failwith ("uppercase_mapping: " ^ Char.escaped c) }


and lowercase_mapping cp c = parse
  | (field as f)? ';'		{ titlecase_mapping cp { c with lowercase_mapping = opt int_of_hex_string f } lexbuf }

  | _ as c			{ failwith ("lowercase_mapping: " ^ Char.escaped c) }


and titlecase_mapping cp c = parse
  | (field as f)? '\n'		{ codepoint cp { c with titlecase_mapping = opt int_of_hex_string f } lexbuf }

  | _ as c			{ failwith ("titlecase_mapping: " ^ Char.escaped c) }


{
  let codepoint_list lexbuf =
    List.tl (List.rev (codepoint [] empty lexbuf))

  let codepoint_array lexbuf =
    let cplist = codepoint [] empty lexbuf in
    let cparray = Array.make 0x10ffff empty in
    List.iter (fun cp ->
      cparray.(cp.codepoint) <- cp
    ) (List.tl (List.rev cplist));
    cparray
}
