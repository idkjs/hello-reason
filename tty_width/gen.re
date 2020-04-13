/*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

let next_uchar = u =>
  if (Uchar.equal(u, Uchar.max)) {
    None;
  } else {
    Some(Uchar.succ(u));
  };
let utf_8_of_uchar = u => {
  let b = Buffer.create(4);
  Buffer.add_utf_8_uchar(b, u);
  Buffer.contents(b);
};

type map =
  | Z
  | O
  | T
  | Byte(array(map));

let rec pp_map = ppf =>
  fun
  | Z => Format.pp_print_string(ppf, "Z")
  | O => Format.pp_print_string(ppf, "O")
  | T => Format.pp_print_string(ppf, "T")
  | Byte(b) => {
      let pp_els = (ppf, b) =>
        for (i in 0 to 255) {
          Format.fprintf(ppf, "%a;@,", pp_map, b[i]);
        };

      Format.fprintf(ppf, "Byte @[<2>[|%a|]@]", pp_els, b);
    };

let width = (m, s, ~start) => {
  let rec loop = (s, max, i) =>
    fun
    | Byte(b) =>
      if (i > max) {
        1;
      } else {
        loop(s, max, i + 1, b[Char.code(s.[i])]);
      }
    | Z => 0
    | O => 1
    | T => 2;

  let i =
    if (start < 0) {
      0;
    } else {
      start;
    };
  loop(s, String.length(s) - 1, i, m);
};

let list = () => {
  let rec loop = acc =>
    fun
    | None => List.rev(acc)
    | Some(u) => {
        let v =
          switch (Uucp.Break.tty_width_hint(u)) {
          | (-1) => Z
          | 0 => Z
          | 1 => O
          | 2 => T
          | _ => assert(false)
          };

        loop([(utf_8_of_uchar(u), v), ...acc], next_uchar(u));
      };

  loop([], Some(Uchar.min));
};

let compress = m => {
  let rec loop =
    fun
    | Z => Z
    | O => O
    | T => T
    | Byte(b) => {
        for (i in 0 to Array.length(b) - 1) {
          b[i] = loop(b[i]);
        };
        let w = b[0];
        try(
          {
            for (i in 0 to Array.length(b) - 1) {
              if (b[i] != w) {
                raise(Exit);
              };
            };
            w;
          }
        ) {
        | Exit => Byte(b)
        };
      };

  loop(m);
};

let map_of_list = l => {
  let new_byte = () => Byte(Array.make(256, O));
  let rec add = (s, v, m) => {
    let rec loop = (s, max, i, v, m) =>
      if (i > max) {
        v;
      } else {
        switch (m) {
        | O => loop(s, max, i, v, new_byte())
        | Byte(b) as m =>
          let byte = Char.code(s.[i]);
          b[byte] = loop(s, max, i + 1, v, b[byte]);
          m;
        | T => assert(false)
        | Z =>
          Printf.printf("utf:%s len:%d i:%d\n%!", s, max + 1, i);
          assert(false);
        };
      };

    loop(s, String.length(s) - 1, 0, v, m);
  };

  let rec loop = m =>
    fun
    | [] => m
    | [(utf_8, v), ...acc] => loop(add(utf_8, v, m), acc);

  loop(new_byte(), l);
};

let assert_map = m => {
  let rec loop =
    fun
    | None => ()
    | Some(u) => {
        let w = width(m, utf_8_of_uchar(u), ~start=0);
        let uucp_w =
          switch (Uucp.Break.tty_width_hint(u)) {
          | (-1) => 0
          | w => w
          };

        if (!(w == uucp_w)) {
          Printf.printf(
            "FAIL: U+%04X w:%d uucp:%d\n%!",
            Uchar.to_int(u),
            w,
            uucp_w,
          );
          assert(false);
        };
        loop(next_uchar(u));
      };

  loop(Some(Uchar.min));
};

let assert_first_non_ascii_byte = m =>
  for (i in 0x80 to 0xFF) {
    assert(width(m, String.make(1, Char.chr(i)), ~start=0) == 1);
  };

let width_stats = () => {
  let rec loop = (z, o, t) =>
    fun
    | None => (z, o, t)
    | Some(u) => {
        let next = next_uchar(u);
        switch (Uucp.Break.tty_width_hint(u)) {
        | (-1) => loop(z + 1, o, t, next)
        | 0 => loop(z + 1, o, t, next)
        | 1 => loop(z, o + 1, t, next)
        | 2 => loop(z, o, t + 1, next)
        | _ => assert(false)
        };
      };

  loop(0, 0, 0, Some(Uchar.min));
};

let kb_size = v => {
  let words = Obj.reachable_words(Obj.repr(v));
  words / (Sys.word_size / 8) / 1024;
};

let test = () => {
  let (z, o, t) = width_stats();
  Printf.printf("z:%d o:%d t:%d\n%!", z, o, t);
  let l = list();
  Printf.printf("list: %dKiB\n%!", kb_size(l));
  let m = map_of_list(l);
  Printf.printf("map: %dKiB\n%!", kb_size(m));
  assert_map(m);
  let cm = compress(m);
  Printf.printf("compressed map: %dKiB\n%!", kb_size(cm));
  assert_map(cm);
  assert_first_non_ascii_byte(cm);
  ();
};

let gen = () => {
  let cm = compress(map_of_list(list()));
  assert_map(cm);
  assert_first_non_ascii_byte(cm);
  let map_src = Format.asprintf("%a", pp_map, cm);
  Printf.printf(
    {|(*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
let unicode_version = "%s"

type map = Z | O | T | Byte of map array
let map =
(* Do not edit. Data generated by tty_width/gen.ml *)
%s

let of_utf_8 s ~start =
  let rec loop s max i = function
  | Byte b ->
      if i > max then 1 else
      loop s max (i + 1) b.(Char.code s.[i])
  | Z -> 0 | O -> 1 | T -> 2
  in
  let i = if start < 0 then 0 else start in
  loop s (String.length s - 1) i map

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
%!|},
    Uucp.unicode_version,
    map_src,
  );
};

let () =
  switch (Array.to_list(Sys.argv)) {
  | [_, "-t"] => test()
  | [_] => gen()
  | _ => Printf.printf("Usage: %s [-t]\n%!", Sys.argv[0])
  };

/*---------------------------------------------------------------------------
   Copyright (c) 2019 The down programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
