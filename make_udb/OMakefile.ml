install Program ".DEFAULT" [
  (* Target *)
  Name		"make_udb";

  (* Sources *)
  Modules [
    "Make_udb";
    "UcsTest";
    "Udb_lexer";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "batteries";
    "ucs";
  ];
]
