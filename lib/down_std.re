/*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

let strf = Printf.sprintf;

/* Result values */

module Result = {
  let catch_sys_error = fn =>
    try(fn()) {
    | Sys_error(e) => Error(e)
    };
  let map_error = f =>
    fun
    | Ok(_) as v => v
    | Error(e) => Error(f(e));
  let bind = (r, f) =>
    switch (r) {
    | Ok(v) => f(v)
    | Error(_) as e => e
    };
};

/* Tries */

module Trie = {
  module type S = {
    type elt;
    type t('a);
    let is_empty: t('a) => bool;
    let empty: t('a);
    let value: t('a) => option('a);
    let add: (list(elt), option('a), t('a)) => t('a);
    let find: (list(elt), t('a)) => t('a);
    let find_fork: (list(elt), t('a)) => (list(elt), t('a));
  };
  module Make = (T: Map.OrderedType) => {
    type elt = T.t;
    module Tmap = Map.Make(T);
    type t('a) = {
      v: option('a),
      succs: Tmap.t(t('a)),
    };
    let empty = {v: None, succs: Tmap.empty};
    let is_empty = t => t.v == None && Tmap.is_empty(t.succs);
    let value = t => t.v;
    let add = (es, v, t) => {
      let rec loop = (es, v, t) =>
        switch (es) {
        | [] => {...t, v}
        | [e, ...es] =>
          let t' =
            try(Tmap.find(e, t.succs)) {
            | Not_found => empty
            };
          let succs = Tmap.add(e, loop(es, v, t'), t.succs);
          {...t, succs};
        };

      loop(es, v, t);
    };

    let rec find = (es, t) =>
      switch (es) {
      | [] => t
      | [e, ...es] =>
        switch (Tmap.find(e, t.succs)) {
        | exception Not_found => empty
        | t => find(es, t)
        }
      };

    let find_fork = (es, t) => {
      let is_fork = t => t.v != None || Tmap.cardinal(t.succs) != 1;
      let rec find_es = (acc, es, t) =>
        switch (es) {
        | [] => (acc, t)
        | [e, ...es] =>
          switch (Tmap.find(e, t.succs)) {
          | exception Not_found => (acc, empty)
          | t => find_es([e, ...acc], es, t)
          }
        };

      let rec kontinue = (acc, t) =>
        is_fork(t)
          ? (List.rev(acc), t)
          : (
            switch (Tmap.choose(t.succs)) {
            | exception Not_found => assert(false)
            | (e, t) => kontinue([e, ...acc], t)
            }
          );

      let (acc, t) = find_es([], es, t);
      kontinue(acc, t);
    };
  };
};

/* Seemingly UTF-8 text. */

module Txt = {
  let find_next = (~sat, s, ~start) => {
    let rec loop = (s, max, i) =>
      if (i > max) {
        String.length(s);
      } else if (sat(s.[i])) {
        i;
      } else {
        loop(s, max, i + 1);
      };

    let max = String.length(s) - 1
    and i =
      if (start < 0) {
        0;
      } else {
        start;
      };
    loop(s, max, i);
  };

  let find_prev = (~sat, s, ~start) => {
    let rec loop = (s, i) =>
      if (i < 0) {
        0;
      } else if (sat(s.[i])) {
        i;
      } else {
        loop(s, i - 1);
      };

    let max = String.length(s) - 1;
    let i =
      if (start > max) {
        max;
      } else {
        start;
      };
    loop(s, i);
  };

  let keep_next_len = (~sat, s, ~start) => {
    let rec loop = (s, max, i, count) =>
      if (i > max) {
        count;
      } else if (sat(s.[i])) {
        loop(s, max, i + 1, count + 1);
      } else {
        count;
      };

    let max = String.length(s) - 1
    and i =
      if (start < 0) {
        0;
      } else {
        start;
      };
    loop(s, max, i, 0);
  };

  let keep_prev_len = (~sat, s, ~start) => {
    let rec loop = (s, i, count) =>
      if (i < 0) {
        count;
      } else if (sat(s.[i])) {
        loop(s, i - 1, count + 1);
      } else {
        count;
      };

    let max = String.length(s) - 1;
    let i =
      if (start > max) {
        max;
      } else {
        start;
      };
    loop(s, i, 0);
  };

  /* Lines */

  let lines = s => {
    /* adapted from the stdlib's String.split_on_char, handles CR, CRLF and
       LF line ends. */
    let r = ref([]);
    let j = ref(String.length(s));
    for (i in String.length(s) - 1 downto 0) {
      if (String.unsafe_get(s, i) == '\n') {
        r := [String.sub(s, i + 1, j^ - i - 1), ...r^];
        j :=
          (
            if (i != 0 && String.unsafe_get(s, i - 1) == '\r') {
              i - 1;
            } else {
              i;
            }
          );
      };
    };
    [String.sub(s, 0, j^), ...r^];
  };

  /* XXX something smarter should likely be done for CRFL management here...
     XXX Maybe look only for '\n' will do ? */

  let is_eol =
    fun
    | '\n'
    | '\r' => true
    | _ => false;
  let find_next_eol = (s, ~start) => find_next(~sat=is_eol, s, ~start);
  let find_prev_eol = (s, ~start) => find_prev(~sat=is_eol, s, ~start);
  let find_prev_sol = (s, ~start) => {
    let i = find_prev_eol(s, ~start);
    if (String.length(s) == 0 || i == 0 && !is_eol(s.[i])) {
      0;
    } else {
      i + 1;
    };
  };

  /* UTF-8 uchars */

  let utf_8_decode_len = c =>
    switch (Char.code(c)) {
    | b when b <= 0x7F => 1
    | b when b <= 0xBF => 1
    | b when b <= 0xDF => 2
    | b when b <= 0xEF => 3
    | b when b <= 0xF7 => 4
    | _ => 1
    };

  let is_utf_8_decode = c => Char.code(c) land 0xC0 != 0x80;
  let find_next_utf_8_decode = (s, ~start) =>
    find_next(~sat=is_utf_8_decode, s, ~start);
  let find_prev_utf_8_decode = (s, ~start) =>
    find_prev(~sat=is_utf_8_decode, s, ~start);

  /* White */

  let is_white =
    fun
    | ' '
    | '\t'
    | '\n'
    | '\011'
    | '\012'
    | '\r' => true
    | _ => false;

  let find_next_white = (s, ~start) => find_next(~sat=is_white, s, ~start);
  let find_prev_white = (s, ~start) => find_prev(~sat=is_white, s, ~start);

  /* In general it's unwise to use the following functions on UTF-8
     since they might end up on continuation bytes. But it's ok the
     way we use them for segmenting words below. */

  let is_non_white = c => !is_white(c);
  let find_next_non_white = (s, ~start) =>
    find_next(~sat=is_non_white, s, ~start);
  let find_prev_non_white = (s, ~start) =>
    find_prev(~sat=is_non_white, s, ~start);

  /* Words */

  let find_next_after_eow = (s, ~start) =>
    find_next_white(s, ~start=find_next_non_white(s, ~start));

  let find_prev_sow = (s, ~start) => {
    let i = find_prev_white(s, ~start=find_prev_non_white(s, ~start));
    if (String.length(s) == 0 || i == 0 && is_non_white(s.[i])) {
      0;
    } else {
      i + 1;
    };
  };

  /* Grapheme clusters and TTY width */

  let tty_width = (s, ~start) =>
    s.[start] == '\n' ? 1 : Down_tty_width.of_utf_8(s, ~start);

  let find_next_gc_and_tty_width = (s, ~after) => {
    let rec loop = (s, max, w, i) =>
      if (i > max) {
        (String.length(s), w);
      } else {
        let iw = tty_width(s, ~start=i);
        if (w >= 1 && iw != 0) {
          (i, w);
        } else {
          loop(s, max, w + iw, i + utf_8_decode_len(s.[i]));
        };
      };

    let max = String.length(s) - 1
    and i =
      if (after < 0) {
        0;
      } else {
        after;
      };
    loop(s, max, 0, i);
  };

  let find_next_gc = (s, ~after) =>
    fst(find_next_gc_and_tty_width(s, ~after));
  let find_prev_gc = (s, ~before) => {
    let rec loop = (s, w, i) =>
      if (i <= 0) {
        0;
      } else {
        let i = find_prev_utf_8_decode(s, ~start=i - 1);
        let w = w + tty_width(s, ~start=i);
        if (w >= 1) {
          i;
        } else {
          loop(s, w, i);
        };
      };

    let len = String.length(s);
    let i =
      if (before > len) {
        len;
      } else {
        before;
      };
    loop(s, 0, i);
  };

  let find_prev_eol_and_tty_width = (s, ~before) => {
    let rec loop = (s, w, i) =>
      if (i <= 0) {
        (0, w);
      } else {
        let i = find_prev_utf_8_decode(s, ~start=i - 1);
        if (is_eol(s.[i])) {
          (i, w);
        } else {
          let w = w + tty_width(s, ~start=i);
          loop(s, w, i);
        };
      };

    let len = String.length(s);
    let i =
      if (before > len) {
        len;
      } else {
        before;
      };
    loop(s, 0, i);
  };

  let find_next_tty_width_or_eol = (s, ~start, ~w) => {
    let rec loop = (s, max, w, i) =>
      if (i > max) {
        String.length(s);
      } else if (w <= 0 || is_eol(s.[i])) {
        i;
      } else {
        let (i, gc_w) = find_next_gc_and_tty_width(s, ~after=i);
        loop(s, max, w - gc_w, i);
      };

    let max = String.length(s) - 1
    and i =
      if (start < 0) {
        0;
      } else {
        start;
      };
    loop(s, max, w, i);
  };
};

/* Text made of entries separated by a special line. */

module Txt_entries = {
  let nl = if (Sys.win32) {"\r\n"} else {"\n"};
  let to_string = (~sep, es) =>
    String.concat(Printf.sprintf("%s%s%s", nl, sep, nl), es);
  let of_string = (~sep, s) => {
    let add_entry = (acc, lines) => {
      let e = String.concat(nl, List.rev(lines));
      if (e == "") {
        acc;
      } else {
        [e, ...acc];
      };
    };

    let rec loop = (acc, curr) =>
      fun
      | [] => List.rev(add_entry(acc, curr))
      | [l, ...ls] =>
        if (String.equal(String.trim(l), sep)) {
          loop(add_entry(acc, curr), [], ls);
        } else {
          loop(acc, [l, ...curr], ls);
        };

    loop([], [], Txt.lines(s));
  };
};

/* OS interaction */

let cmd_run = (~stdout=?, ~stderr=?, cmd) => {
  let err = (exit, cmd) =>
    [@implicit_arity] Error(exit, strf("exited with %d: %s\n", exit, cmd));
  let line = (~stdout=?, ~stderr=?, cmd) => {
    let cmd = List.map(Filename.quote, cmd);
    let cmd = String.concat(" ", cmd);
    let redirect = (fd, f) => strf(" %d>%s", fd, Filename.quote(f));
    let stdout =
      switch (stdout) {
      | None => ""
      | Some(f) => redirect(1, f)
      };
    let stderr =
      switch (stderr) {
      | None => ""
      | Some(f) => redirect(2, f)
      };
    let win_quote = if (Sys.win32) {"\""} else {""};
    strf("%s%s%s%s%s", win_quote, cmd, stdout, stderr, win_quote);
  };

  let line = line(~stdout?, ~stderr?, cmd);
  let exit = Sys.command(line);
  if (exit == 0) {
    Ok();
  } else {
    err(exit, line);
  };
};

module Env = {
  let get = var =>
    switch (Sys.getenv(var)) {
    | "" => None
    | exception Not_found => None
    | value => Some(value)
    };
};

module Dir = {
  let config = () =>
    switch (Env.get("XDG_CONFIG_HOME")) {
    | Some(h) => Ok(h)
    | None =>
      switch (
        if (Sys.win32) {
          Env.get("%APPDATA%");
        } else {
          None;
        }
      ) {
      | Some(h) => Ok(h)
      | None =>
        switch (Env.get("HOME")) {
        | Some(h) => Ok(Filename.concat(h, ".config"))
        | None => Error("Could not determine a user configuration directory")
        }
      }
    };

  let mkdir_win32 = dir => ["mkdir", dir];
  let mkdir_posix = dir => ["mkdir", "-p", dir];
  let mkdir = if (Sys.win32) {mkdir_win32} else {mkdir_posix};
  let create = dir => Result.map_error(snd) @@ cmd_run(mkdir(dir));

  let exists = dir =>
    Result.catch_sys_error @@
    (() => Ok(Sys.(file_exists(dir) && is_directory(dir))));

  let contents = dir =>
    Result.catch_sys_error @@
    (
      () => {
        let contents = Array.to_list(Sys.readdir(dir));
        Ok(List.rev_map(Filename.concat(dir), contents));
      }
    );
};

module File = {
  let null =
    switch (Sys.os_type) {
    | "Win32" => "NUL"
    | _ => "/dev/null"
    };
  let rename = (src, ~dst) =>
    Result.catch_sys_error @@ (() => Ok(Sys.rename(src, dst)));

  let delete = file =>
    Result.catch_sys_error @@ (() => Ok(Sys.remove(file)));

  let exists = file =>
    Result.catch_sys_error @@ (() => Ok(Sys.file_exists(file)));

  let with_io_chan = (close, file, chan, fn) =>
    try({
      let r = fn(chan);
      close(chan);
      Ok(r);
    }) {
    | e =>
      try(ignore(close(chan))) {
      | Sys_error(_) => ()
      };
      switch (e) {
      | Sys_error(err) => Error(strf("%s: %s", file, err))
      | End_of_file => Error(strf("%s: unexpected end of file", file))
      | e => raise(e)
      };
    };

  let with_open_in = (file, fn) =>
    Result.catch_sys_error @@
    (
      () => {
        let ic = open_in_bin(file);
        with_io_chan(close_in, file, ic, fn);
      }
    );

  let with_open_out = (file, fn) =>
    Result.catch_sys_error @@
    (
      () => {
        let oc = open_out_bin(file);
        with_io_chan(close_out, file, oc, fn);
      }
    );

  let read = file =>
    with_open_in(file) @@
    (
      ic => {
        let len = in_channel_length(ic);
        let buf = Bytes.create(len);
        really_input(ic, buf, 0, len);
        Bytes.unsafe_to_string(buf);
      }
    );

  let write = (~file, s) =>
    with_open_out(file) @@ (oc => output_string(oc, s));

  let set_content = (~file, s) =>
    Result.bind(exists(file)) @@
    (
      fun
      | true => {
          let old = file ++ ".tmp";
          Result.bind(rename(file, ~dst=old)) @@
          (() => Result.bind(write(~file, s)) @@ (() => delete(old)));
        }
      | false =>
        Result.bind(Dir.create(Filename.dirname(file))) @@
        (() => write(~file, s))
    );

  let tmp = (~suff="", ()) =>
    Result.catch_sys_error @@
    (
      () => {
        let tmp = Filename.temp_file("ocaml", suff);
        at_exit(() =>
          try(Sys.remove(tmp)) {
          | Sys_error(e) => ()
          }
        );
        Ok(tmp);
      }
    );
};

module Cmd = {
  type tool = string;
  type t = list(string);
  let of_string = s => {
    let rec cleanup = acc =>
      fun
      | ["", ...args] => cleanup(acc, args)
      | [a, ...args] => cleanup([String.trim(a), ...acc], args)
      | [] => List.rev(acc);

    cleanup([], String.split_on_char(' ', s));
  };

  let run = cmd_run;
  let test_cmd =
    if (Sys.win32) {
      ["where"];
    } else {
      ["command", "-v"];
    };

  let exists = cmd =>
    switch (cmd) {
    | [] => Ok(false)
    | [tool, ..._] =>
      switch (run(~stdout=File.null, ~stderr=File.null, test_cmd @ [tool])) {
      | Ok () => Ok(true)
      | _ => Ok(false)
      }
    };

  let must_exist = cmd =>
    Result.bind(exists(cmd)) @@
    (
      fun
      | true => Ok(cmd)
      | false => Error(strf("%s: no such command", List.hd(cmd)))
    );

  let read = cmd => {
    let exitify = Result.map_error(s => (255, s));
    Result.bind(exitify @@ File.tmp(~suff="stdout", ())) @@
    (
      stdout =>
        Result.bind(run(~stdout, cmd)) @@
        (() => exitify(File.read(stdout)))
    );
  };
};

module Tty = {
  /* Terminal capabilities */

  type cap = [ | `None | `Ansi];
  let find_cap = () =>
    switch (Sys.getenv("TERM")) {
    | exception Not_found => `None
    | "dumb"
    | "" => `None
    | _ => `Ansi
    };

  let cap = find_cap();

  /* ANSI escapes and styling */

  type color = [
    | `Default
    | `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
  ];

  let rec sgr_base_int_of_color =
    fun
    | `Black => 0
    | `Red => 1
    | `Green => 2
    | `Yellow => 3
    | `Blue => 4
    | `Magenta => 5
    | `Cyan => 6
    | `White => 7
    | `Default => 9
    | `Hi(#color as c) => 60 + sgr_base_int_of_color(c);

  let sgr_of_fg_color = c => strf("%d", 30 + sgr_base_int_of_color(c));
  let sgr_of_bg_color = c => strf("%d", 40 + sgr_base_int_of_color(c));

  type style = [
    | `Bold
    | `Faint
    | `Italic
    | `Underline
    | `Reverse
    | `Fg([ color | `Hi(color)])
    | `Bg([ color | `Hi(color)])
  ];

  let sgr_of_style =
    fun
    | `Bold => "01"
    | `Faint => "02"
    | `Italic => "03"
    | `Underline => "04"
    | `Reverse => "07"
    | `Fg(c) => sgr_of_fg_color(c)
    | `Bg(c) => sgr_of_bg_color(c);

  let sgrs_of_styles = styles =>
    String.concat(";", List.map(sgr_of_style, styles));
  let styled_str = (cap, styles, s) =>
    switch (cap) {
    | `None => s
    | `Ansi => strf("\027[%sm%s\027[m", sgrs_of_styles(styles), s)
    };

  /* Terminal output */

  let output = s => {
    print_string(s);
    flush(stdout);
  };
  let ding = "\007";
  let newline = "\r\n";
  let clear_row = "\027[2K";
  let cursor_up = n =>
    if (n == 0) {
      "";
    } else {
      String.concat("", ["\027[", string_of_int(n), "A"]);
    };

  let cursor_down = n =>
    if (n == 0) {
      "";
    } else {
      String.concat("", ["\027[", string_of_int(n), "B"]);
    };

  let cursor_forward = n =>
    if (n == 0) {
      "";
    } else {
      String.concat("", ["\027[", string_of_int(n), "C"]);
    };

  let cursor_origin = "\027[H";
  let clear_screen = "\027[2J";

  /* Terminal input */

  type arrow = [ | `Up | `Down | `Left | `Right];
  type input = [
    | `Arrow([ | `Up | `Down | `Left | `Right])
    | `Backspace
    | `Bytes(string)
    | `Ctrl([ | `Key(int) | `Arrow(arrow)])
    | `Delete
    | `End
    | `Enter
    | `Escape
    | `Function(int)
    | `Home
    | `Meta(int)
    | `Page([ | `Up | `Down])
    | `Shift([ | `Arrow(arrow)])
    | `Tab
    | `Unknown(string)
  ];

  let pp_input = (ppf, i) => {
    let pp = Format.fprintf;
    let dir_to_string =
      fun
      | `Left => "left "
      | `Right => "right"
      | `Up => "up   "
      | `Down => "down ";

    let pp_char = (ppf, c) =>
      switch (c) {
      | c when c <= 0x20 || c >= 0x80 => pp(ppf, "\\x%02X", c)
      | 0x7F => pp(ppf, "backspace")
      | c => pp(ppf, "%c", Char.chr(c))
      };

    switch (i) {
    | `Arrow(dir) => pp(ppf, "%s", dir_to_string(dir))
    | `Backspace => pp(ppf, "backspace")
    | `Bytes(b) =>
      if (String.length(b) == 1 && Char.code(b.[0]) < 0x20) {
        pp(ppf, "\"\\x%02X\"", Char.code(b.[0]));
      } else {
        pp(ppf, "\"%s\"", b);
      }
    | `Ctrl(`Key(c)) => pp(ppf, "C-%a", pp_char, c)
    | `Ctrl(`Arrow(dir)) => pp(ppf, "C-%s", dir_to_string(dir))
    | `Delete => pp(ppf, "delete")
    | `End => pp(ppf, "end")
    | `Enter => pp(ppf, "enter")
    | `Escape => pp(ppf, "escape")
    | `Function(n) => pp(ppf, "f%d", n)
    | `Home => pp(ppf, "home")
    | `Meta(c) => pp(ppf, "M-%a", pp_char, c)
    | `Page(dir) => pp(ppf, "page-%s", dir_to_string(dir))
    | `Tab => pp(ppf, "tab")
    | `Shift(`Arrow(dir)) => pp(ppf, "shift-%s", dir_to_string(dir))
    | `Unknown(s) => pp(ppf, "unknown (%S)", s)
    };
  };

  let read_esc = (readc): input =>
    switch (readc()) {
    | None => `Escape
    | Some(0x5B) /* '[' */ =>
      switch (readc()) {
      | None => `Unknown(strf("ESC["))
      | Some(0x41) => `Arrow(`Up)
      | Some(0x42) => `Arrow(`Down)
      | Some(0x43) => `Arrow(`Right)
      | Some(0x44) => `Arrow(`Left)
      | Some(0x46) => `End
      | Some(0x48) => `Home
      | Some(0x31) =>
        let pre = "ESC[1";
        switch (readc()) {
        | None => `Unknown(pre)
        | Some(0x3B) /* ; */ =>
          switch (readc()) {
          | None => `Unknown(strf("%s;", pre))
          | Some((0x35 | 0x32) as m) =>
            let mk = dir =>
              if (m == 0x35) {
                `Ctrl(`Arrow(dir));
              } else {
                `Shift(`Arrow(dir));
              };

            switch (readc()) {
            | None => `Unknown(strf("%s;%c", pre, Char.chr(m)))
            | Some(0x41) => mk(`Up)
            | Some(0x42) => mk(`Down)
            | Some(0x43) => mk(`Right)
            | Some(0x44) => mk(`Left)
            | Some(b) =>
              `Unknown(strf("%s;%c%c", pre, Char.chr(m), Char.chr(b)))
            };
          | Some(b) => `Unknown(strf("%s;%c", pre, Char.chr(b)))
          }
        | Some(b) => `Unknown(strf("%s%c", pre, Char.chr(b)))
        };
      | Some(0x33) =>
        let pre = "ESC[3";
        switch (readc()) {
        | None => `Unknown(pre)
        | Some(0x7E) => `Delete
        | Some(b) => `Unknown(strf("%s%c", pre, Char.chr(b)))
        };
      | Some(b) => `Unknown(strf("ESC[%c", Char.chr(b)))
      }
    | Some(0x4F) =>
      switch (readc()) {
      | None => `Unknown(strf("ESC G"))
      | Some(b) when 0x50 <= b && b <= 0x53 => `Function(b - 0x4F)
      | Some(b) => `Unknown(strf("ESC G %c", Char.chr(b)))
      }
    | Some(b) when 0x20 <= b || b < 0x7F => `Meta(b)
    | Some(b) => `Unknown(strf("ESC %02X", b))
    };

  let read_bytes = (readc, first) => {
    let rec loop = (buf, i) =>
      switch (i < Bytes.length(buf)) {
      | false => Bytes.unsafe_to_string(buf)
      | true =>
        switch (readc()) {
        | None => Bytes.sub_string(buf, 0, i)
        | Some(b) =>
          Bytes.set(buf, i, Char.chr(b));
          loop(buf, i + 1);
        }
      };

    let first = Char.chr(first);
    let buf = Bytes.create(Txt.utf_8_decode_len(first));
    Bytes.set(buf, 0, first);
    loop(buf, 1);
  };

  let input = readc =>
    switch (readc()) {
    | None => None
    | Some(b) =>
      let i =
        switch (b) {
        | 0x09 => `Tab
        | 0x0A => `Bytes("\n")
        | 0x0D => `Enter
        | 0x1B => read_esc(readc)
        | 0x7F
        | 0x08 => `Backspace
        | b when b <= 0x1F => `Ctrl(`Key(b + 0x60))
        | b => `Bytes(read_bytes(readc, b))
        };

      Some(i);
    };

  /* Terminal width */

  let width = readc => {
    /* defaults to 80 if something seems wrong */
    let read_pos = readc => {
      let rec rreadc = () =>
        switch (readc()) {
        | None => rreadc()
        | Some(c) => c
        };
      let rec int = (~stop, acc) =>
        switch (rreadc()) {
        | b when 0x30 <= b && b <= 0x39 => int(~stop, 10 * acc + (b - 0x30))
        | b when b == stop => acc
        | _ => 0
        };

      if (rreadc() != 0x1B) {
        (0, 0);
      } else if (rreadc() != 0x5B) {
        (0, 0);
      } else {
        let row = int(~stop=0x3B, 0);
        let col = int(~stop=0x52, 0);
        (col, row);
      };
    };

    let get_cursor_pos = readc => {
      output("\027[6n");
      read_pos(readc);
    };
    try({
      let (x0, _) = get_cursor_pos(readc);
      output("\027[100000C"); /* go far far right */
      let (x1, _) = get_cursor_pos(readc);
      if (x1 > x0) {
        output(strf("\027[%dD", x1 - x0));
      }; /* go back */
      if (x1 == 0) {
        raise(Exit);
      } else {
        x1;
      };
    }) {
    | Sys_error(_)
    | Exit => 80
    };
  };
};

module Stdin = {
  external set_raw_mode: bool => bool = "ocaml_down_stdin_set_raw_mode";
  external readc: unit => int = "ocaml_down_stdin_readc";
  let readc = () =>
    switch (readc()) {
    | (-1)
    | (-2) => None
    | (-3) => raise(Sys_error("stdin read error"))
    | n => Some(n)
    };

  let () = {
    let disable_raw = () => ignore(set_raw_mode(false));
    at_exit(disable_raw);
  };
};

module Fmt = {
  type t('a) = (Format.formatter, 'a) => unit;
  let pf = Format.fprintf;
  let pr = Format.printf;
  let kpf = Format.kfprintf;
  let str = Format.asprintf;
  let string = Format.pp_print_string;
  let sp = (ppf, _) => Format.pp_print_space(ppf, ());
  let any = (fmt, ppf, _) => pf(ppf, fmt);
  let list = (~sep as pp_sep=?, pp_v) =>
    Format.pp_print_list(~pp_sep?, pp_v);
  let text = Format.pp_print_text;
  let tty = (styles, pp_v, ppf, v) =>
    switch (Tty.cap) {
    | `None => pp_v(ppf, v)
    | `Ansi =>
      let reset = ppf => pf(ppf, "@<0>%s", "\027[m");
      let styles = Tty.sgrs_of_styles(styles);
      kpf(reset, ppf, "@<0>%s%a", strf("\027[%sm", styles), pp_v, v);
    };
};

module Editor = {
  let find = () =>
    switch (Env.get("EDITOR")) {
    | Some(cmd) => Ok(Cmd.of_string(cmd))
    | None =>
      switch (Env.get("VISUAL")) {
      | Some(cmd) => Ok(Cmd.of_string(cmd))
      | None => Error("No editor found in VISUAL or EDITOR env vars.")
      }
    };

  let edit_string = (~ext, s) =>
    Result.bind(find()) @@
    (
      editor =>
        Result.bind(File.tmp(~suff=ext, ())) @@
        (
          tmp =>
            Result.bind(
              if (s == "") {
                Ok();
              } else {
                File.write(tmp, s);
              },
            ) @@
            (
              () =>
                Result.bind(
                  Result.map_error(snd) @@ Cmd.run(editor @ [tmp]),
                ) @@
                (
                  () =>
                    Result.bind(File.read(tmp)) @@
                    (txt => Ok(String.trim(txt)))
                )
            )
        )
    );

  let edit_file = file =>
    Result.bind(find()) @@
    (editor => Result.map_error(snd) @@ Cmd.run(editor @ [file]));
};

/*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers

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
