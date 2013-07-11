(*************************************************
 * :: Recursive definition of the above two
 *************************************************)

module Make(UTF : Encoding.Scheme) : Encoding.S = struct

  let name = UTF.name

  module rec String : Encoding.String = struct

    type t = string

    let compare = Pervasives.compare
    let equal = Pervasives.(=)

    let rec fold_left i f x s =
      if ByteString.length s - i > 0 then
        let (cp, advance) = UTF.get s i in
        fold_left (i + advance) f (f x cp) s
      else
        x

    let fold_left f x s =
      fold_left 0 f x s

    let map f s =
      let mapped = Buffer.create (ByteString.length s) in
      fold_left (fun () cp ->
        Buffer.add_char mapped (f cp)
      ) () s;
      Buffer.contents mapped

    let mapi f s =
      let mapped = Buffer.create (ByteString.length s) in
      ignore (fold_left (fun i cp ->
        Buffer.add_char mapped (f i cp);
        i + 1
      ) 0 s);
      Buffer.contents mapped

    let unsafe_adopt s = s

    let adopt s =
      fold_left (fun () _ -> ()) () s;
      s

  end
  and Buffer : Encoding.Buffer with type data = String.t = struct

    type t = {
      mutable buf : string;
      mutable pos : int;
    }

    type data = String.t

    let create len = {
      buf = ByteString.make len '\xff';
      pos = 0;
    }

    let add_char b cp =
      (* Make sure at least 6 bytes fit in. That is
       * the maximum for UTF-8 encoded code points. *)
      if b.pos + 6 > ByteString.length b.buf then (
        let buf = ByteString.create (ByteString.length b.buf * 2 + 6) in
        ByteString.blit b.buf 0 buf 0 b.pos;
        b.buf <- buf;
      );
      b.pos <- b.pos + UTF.set b.buf b.pos cp

    let contents b =
      String.unsafe_adopt (ByteString.sub b.buf 0 b.pos)

  end
end
