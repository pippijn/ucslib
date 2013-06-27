open Codepoint

let count = Array.length Udb_data.data

let by f =
  let table = lazy (
    let table = Hashtbl.create count in
    Array.iter (fun cp -> Hashtbl.add table (f cp) cp) Udb_data.data;
    table
  ) in
  fun x -> List.sort compare (Hashtbl.find_all (Lazy.force table) x)

let by_codepoint = by (fun cp -> cp.code)
let by_character_name = by (fun cp -> cp.name)
let by_general_category = by (fun cp -> cp.gc)
let by_canonical_combining_classes = by (fun cp -> cp.ccc)
let by_bidirectional_category = by (fun cp -> cp.bc)
let by_character_decomposition_mapping = by (fun cp -> cp.cdm)
