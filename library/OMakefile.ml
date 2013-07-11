install Package ".DEFAULT" [
  (* Target *)
  Name		"ucs";
  Description	"Small unicode library";
  Version	"0.1";

  (* Sources *)
  Modules [
    "ByteString";
    "Codepoint";
    "Encoding";
    "UChar";
    "Udb";
    "Udb_data";
    "Udb_print";
    "Unicode";
    "UTF";
    "UTF_8";
    "UTF_16BE";
    "UTF_16LE";
    "UTF_32BE";
    "UTF_32LE";
    "Util";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
  ];
]
