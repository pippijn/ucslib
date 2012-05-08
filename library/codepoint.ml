type general_category =
  | Cc
  | Cf
  | Co
  | Cs
  | Ll
  | Lm
  | Lo
  | Lt
  | Lu
  | Mc
  | Me
  | Mn
  | Nd
  | Nl
  | No
  | Pc
  | Pd
  | Pe
  | Pf
  | Pi
  | Po
  | Ps
  | Sc
  | Sk
  | Sm
  | So
  | Zl
  | Zp
  | Zs


type bidirectional_category =
  | AL
  | AN
  | B
  | BN
  | CS
  | EN
  | ES
  | ET
  | L
  | LRE
  | LRO
  | NSM
  | ON
  | PDF
  | R
  | RLE
  | RLO
  | S
  | WS


type character_decomposition_mapping =
  | Standard
  | Font
  | NoBreak
  | Initial
  | Medial
  | Final
  | Isolated
  | Circle
  | Super
  | Sub
  | Vertical
  | Wide
  | Narrow
  | Small
  | Square
  | Fraction
  | Compat


type t = {
  codepoint : int;
  character_name : string option;
  general_category : general_category;
  canonical_combining_classes : int;
  bidirectional_category : bidirectional_category;
  character_decomposition_mapping : (character_decomposition_mapping * int list) option;
  decimal_digit_value : string option;
  digit_value : string option;
  numeric_value : string option;
  mirrored : bool;
  unicode_1_0_name : string option;
  comment : string option;
  uppercase_mapping : int option;
  lowercase_mapping : int option;
  titlecase_mapping : int option;
}
