install Package ".DEFAULT" [
  (* Target *)
  Name		"ucs";
  Description	"Small unicode library";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Codepoint";
    "Udb";
    "Udb_data";
    "Udb_print";
    "Unicode";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
  ];
]
