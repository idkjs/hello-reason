/*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

open Down_std;

/* Access toploop API functionality regardless of ocaml or ocamlnat.
   Works around 4. in https://github.com/ocaml/ocaml/issues/7589 */

module type TOP = {
  let read_interactive_input: ref((string, bytes, int) => (int, bool));
  let use_file: (Format.formatter, string) => bool;
  let use_silently: (Format.formatter, string) => bool;
};

module Nil = {
  let nil = () => invalid_arg("Down.Private.set_top not called");
  let read_interactive_input = ref((_, _, _) => nil());
  let use_file = (_, _) => nil();
  let use_silently = (_, _) => nil();
};

/* Set to the right implementation by Down_top or Down_nattop */
let top: ref(module TOP) = (
  ref((module Nil): (module TOP)): ref(module TOP)
);
let original_ocaml_readline = ref((_, _, _) => assert(false));

let use_file = (~silent=false, file) => {
  module Top = (val top^: TOP);
  silent
    ? Top.use_silently(Format.std_formatter, file)
    : Top.use_file(Format.std_formatter, file);
};

/* Logging and formatting styles */

let faint = ref(true);
let add_faint = acc =>
  if (faint^) {
    [`Faint, ...acc];
  } else {
    acc;
  };
let tty_no_faint = () => faint := false;
let style_err = [`Bold, `Fg(`Red)]; /* match ocaml */
let style_warn = [`Bold, `Fg(`Magenta)]; /* match ocaml */
let style_doc_section = [`Fg(`Yellow)];
let style_code = [`Bold];
let style_id_complete_suff = [`Fg(`Hi(`Cyan))];
let style_id_complete_info = () => add_faint([`Italic]);
let style_id_info_id = [`Fg(`Hi(`Cyan))];
let style_id_info_type = [`Italic];
let style_last_indicator = [`Fg(`Yellow)];
let style_key = [`Bold];
let style_prompt = [`Fg(`Hi(`Green))];
let style_prompt_inactive = () => add_faint([`Fg(`Green)]);
let style_prompt_recording = [`Fg(`Hi(`Magenta))];
let style_prompt_recording_inactive = () => add_faint([`Fg(`Magenta)]);
let pp_error = Fmt.tty(style_err, Fmt.string);
let pp_warn = Fmt.tty(style_warn, Fmt.string); /* match ocaml */
let pp_doc_section = Fmt.tty(style_doc_section, Fmt.string);
let pp_code = Fmt.tty(style_code, Fmt.string);
let pp_faint = () => Fmt.tty(add_faint([]), Fmt.string);
let log_error = fmt => Fmt.pr("%a: " ^^ fmt ^^ "@.", pp_error, "Error");
let log_warning = fmt => Fmt.pr("%a: " ^^ fmt ^^ "@.", pp_warn, "Warning");
let log_on_error = (~use) =>
  fun
  | Error(e) => {
      log_error("%s", e);
      use;
    }
  | Ok(v) => v;

/* Prompt history */

module Phistory = {
  type t = {
    prev: list(string),
    focus: string,
    next: list(string),
  };
  let v = prev => {
    let add = (acc, e) =>
      switch (String.trim(e)) {
      | "" => acc
      | e => [e, ...acc]
      };
    {prev: List.rev(List.fold_left(add, [], prev)), focus: "", next: []};
  };

  let empty = v([]);
  let push = (e, es) =>
    if (e == "") {
      es;
    } else {
      switch (es) {
      | [e', ..._] when String.equal(e, e') => es
      | es => [e, ...es]
      };
    };

  let entries = h => {
    let next = List.filter(s => !String.equal(s, ""), h.next);
    List.rev_append(push(h.focus, next), h.prev);
  };

  let add = (h, e) =>
    switch (String.trim(e)) {
    | "" => h
    | e => v(push(e, entries(h)))
    };
  let restart = h => v(entries(h));
  let prev = (h, current) =>
    switch (h.prev) {
    | [] => None
    | [p, ...ps] =>
      let next = push(String.trim(current), push(h.focus, h.next));
      let next =
        if (next == []) {[""];} /* bottom can be empty */ else {
          next;
        };
      Some(({prev: ps, focus: p, next}, p));
    };

  let next = (h, current) =>
    switch (h.next) {
    | [] => None
    | [n, ...ns] =>
      let prev = push(String.trim(current), push(h.focus, h.prev));
      Some(({prev, focus: n, next: ns}, n));
    };

  let to_string = (~sep, h) => Txt_entries.to_string(~sep, entries(h));
  let of_string = (~sep, s) => v(Txt_entries.of_string(~sep, s));
};

/* String editor */

module Pstring = {
  type t = {
    s: string,
    cursor: int,
    mark: option(int),
  };
  let v = (~cursor=0, ~mark=?, s) => {s, cursor, mark};
  let empty = v("");
  let is_empty = p => String.equal(p.s, "");
  let txt = p => p.s;
  let txt_range = (~first as r0, ~last as r1, p) =>
    String.sub(p.s, r0, r1 - r0 + 1);
  let cursor = p => p.cursor;
  let set_cursor = (cursor, p) =>
    if (p.cursor == cursor) {
      p;
    } else {
      {...p, cursor};
    };
  let set_mark = (mark, p) => {...p, mark};
  let swap_cursor_and_mark = p =>
    switch (p.mark) {
    | None => p
    | Some(m) when m > String.length(p.s) => {...p, mark: None}
    | Some(m) => {...p, cursor: m, mark: Some(p.cursor)}
    };

  let soi = p => set_cursor(0, p);
  let eoi = p => set_cursor(String.length(p.s), p);
  let sol = p => set_cursor(Txt.find_prev_sol(p.s, ~start=p.cursor - 1), p);
  let eol = p => set_cursor(Txt.find_next_eol(p.s, ~start=p.cursor), p);
  let next_char = p => set_cursor(Txt.find_next_gc(p.s, ~after=p.cursor), p);
  let prev_char = p =>
    set_cursor(Txt.find_prev_gc(p.s, ~before=p.cursor), p);
  let next_word = p =>
    set_cursor(Txt.find_next_after_eow(p.s, ~start=p.cursor), p);
  let prev_word = p =>
    set_cursor(Txt.find_prev_sow(p.s, ~start=p.cursor - 1), p);

  let prev_line = p => {
    let (l, w) = Txt.find_prev_eol_and_tty_width(p.s, ~before=p.cursor);
    let pl = Txt.find_prev_eol(p.s, ~start=l - 1);
    let start =
      if (pl == l || !Txt.is_eol(p.s.[pl])) {
        0;
      } else {
        pl + 1;
      };
    let cursor = Txt.find_next_tty_width_or_eol(p.s, ~start, ~w);
    set_cursor(cursor, p);
  };

  let next_line = p => {
    let (l, w) = Txt.find_prev_eol_and_tty_width(p.s, ~before=p.cursor);
    let start =
      if (p.cursor == 0 && w == 0) {
        0;
      } else {
        l + 1;
      };
    let nl = Txt.find_next_eol(p.s, ~start);
    if (nl == String.length(p.s)) {
      p;
    } else {
      let cursor = Txt.find_next_tty_width_or_eol(p.s, ~start=nl + 1, ~w);
      set_cursor(cursor, p);
    };
  };

  let subst = (~start as r0, ~stop as r1, bytes, p) => {
    let slen = String.length(p.s);
    let blen = String.length(bytes);
    if (r0 == slen) {
      {...p, s: p.s ++ bytes, cursor: slen + blen};
    } else {
      let b = String.sub(p.s, 0, r0);
      let a = String.sub(p.s, r1, slen - r1);
      let s = String.concat("", [b, bytes, a]);
      let mark =
        switch (p.mark) {
        | None => None
        | Some(m) when m <= slen + blen => p.mark
        | Some(_) => None
        };

      {s, cursor: r0 + blen, mark};
    };
  };

  let insert = (bytes, p) => subst(p.cursor, p.cursor, bytes, p);
  let delete_next_char = p =>
    if (p.cursor == String.length(p.s)) {
      p;
    } else {
      let stop = Txt.find_next_gc(p.s, ~after=p.cursor);
      subst(~start=p.cursor, ~stop, "", p);
    };

  let delete_prev_char = p =>
    if (p.cursor == 0) {
      p;
    } else {
      let prev = Txt.find_prev_gc(p.s, ~before=p.cursor);
      subst(~start=prev, ~stop=p.cursor, "", p);
    };

  let kill_next_word = p => {
    let stop = Txt.find_next_after_eow(p.s, ~start=p.cursor);
    if (stop == p.cursor) {
      (p, None);
    } else {
      let kill = txt_range(~first=p.cursor, ~last=stop - 1, p);
      (subst(~start=p.cursor, ~stop, "", p), Some(kill));
    };
  };

  let kill_prev_word = p =>
    if (p.cursor == 0) {
      (p, None);
    } else {
      let prev = Txt.find_prev_sow(p.s, ~start=p.cursor - 1);
      let kill = txt_range(~first=prev, ~last=p.cursor - 1, p);
      (subst(~start=prev, ~stop=p.cursor, "", p), Some(kill));
    };

  let kill_to_sol = p => {
    let start = Txt.find_prev_sol(p.s, ~start=p.cursor - 1);
    let start =
      if (start == p.cursor) {
        p.cursor - 1;
      } else {
        start;
      };
    let last = p.cursor - 1;
    if (start < 0 || last < 0) {
      (p, None);
    } else {
      let kill = txt_range(~first=start, ~last, p);
      (subst(~start, ~stop=p.cursor, "", p), Some(kill));
    };
  };

  let kill_to_eol = p => {
    let stop = Txt.find_next_eol(p.s, ~start=p.cursor);
    let stop =
      if (stop == p.cursor) {
        stop + 1;
      } else {
        stop;
      };
    if (stop > String.length(p.s)) {
      (p, None);
    } else {
      let kill = txt_range(~first=p.cursor, ~last=stop - 1, p);
      (subst(~start=p.cursor, ~stop, "", p), Some(kill));
    };
  };

  let kill_region = p =>
    switch (p.mark) {
    | None => (p, None)
    | Some(mark) =>
      if (mark > String.length(p.s)) {
        ({...p, mark: None}, None);
      } else if (mark == p.cursor) {
        (p, None);
      } else {
        let (min, max) =
          p.cursor < mark ? (p.cursor, mark - 1) : (mark, p.cursor - 1);

        let kill = txt_range(~first=min, ~last=max, p);
        let cursor = min
        and mark = Some(min);
        (
          {...subst(~start=min, ~stop=max + 1, "", p), cursor, mark},
          Some(kill),
        );
      }
    };

  let geometry = (~tty_w, ~margin_w, p) => {
    /* Returns [rmax], (cr, cc, c_nl). [rmax] and [cr] are zero-based
       row indexes relative to prompt row for: the maximal row and the
       cursor row. [cc] is the cursor column and [c_nl] indicates if a
       newline has to be added for a wrapping cursor. */
    let rec loop = (s, max, cursor, cr, cc, r, c, i) =>
      i > max
        ? if (cursor != String.length(s)) {
            (r, (cr, cc, false));
          } else if (c mod tty_w == 0) {
            (r + 1, (r + 1, 0, true));
          } else {
            (r, (r, c, false));
          }
        : {
          let (r, c) =
            if (c mod tty_w == 0) {
              (/* wrap */ r + 1, 0);
            } else {
              (r, c);
            };
          let (cr, cc) =
            if (i == cursor) {
              (r, c);
            } else {
              (cr, cc);
            };
          let nl = s.[i] == '\n';
          let (i, gc_w) = Txt.find_next_gc_and_tty_width(s, ~after=i);
          let (r, c) =
            if (nl) {
              (r + 1, margin_w);
            } else {
              (r, c + gc_w);
            };
          loop(s, max, cursor, cr, cc, r, c, i);
        };

    loop(p.s, String.length(p.s) - 1, p.cursor, 0, margin_w, 0, margin_w, 0);
  };
};

/* OCaml history */

module History = {
  let sep = "(**)";
  let h = ref(Phistory.v([]));
  let add = txt => h := Phistory.add(h^, txt);
  let restart = () => h := Phistory.restart(h^);
  let prev = current =>
    switch (Phistory.prev(h^, current)) {
    | None => None
    | Some((h', txt)) =>
      h := h';
      Some(txt);
    };

  let next = current =>
    switch (Phistory.next(h^, current)) {
    | None => None
    | Some((h', txt)) =>
      h := h';
      Some(txt);
    };

  let file = () =>
    Result.bind(Dir.config()) @@
    (
      dir => Ok(Filename.concat(dir, Filename.concat("ocaml", "history.ml")))
    );

  let load = () =>
    log_on_error(~use=()) @@
    Result.map_error(Fmt.str("history load failed: %s")) @@
    Result.bind(file()) @@
    (
      file =>
        Result.bind(File.exists(file)) @@
        (
          fun
          | false => Ok()
          | true =>
            Result.bind(File.read(file)) @@
            (hstr => Ok(h := Phistory.of_string(~sep, hstr)))
        )
    );

  let save = () =>
    log_on_error(~use=()) @@
    Result.map_error(Fmt.str("history save failed: %s")) @@
    Result.bind(file()) @@
    (file => File.set_content(~file, Phistory.to_string(~sep, h^)));

  let edit = () =>
    log_on_error(~use=()) @@
    Result.map_error(Fmt.str("history edit failed: %s")) @@
    Result.bind(file()) @@
    (
      file => {
        let hstr = Phistory.to_string(~sep, h^);
        Result.bind(File.set_content(~file, hstr)) @@
        (
          () =>
            Result.bind(Editor.edit_file(file)) @@
            (
              () =>
                Result.bind(File.read(file)) @@
                (hstr => Ok(h := Phistory.of_string(~sep, hstr)))
            )
        );
      }
    );

  let clear = () => {
    h := Phistory.v([]);
    save();
  };
};

/* Sessions */

module Session = {
  type name = string;
  let dir = () =>
    Result.bind(Dir.config()) @@
    (dir => Ok(Filename.concat(dir, Filename.concat("ocaml", "session"))));

  let dir_file = fn =>
    Result.bind(dir()) @@ (d => Ok(Filename.concat(d, fn)));
  let last_session_file = () => dir_file("last");
  let unsaved_file = () => dir_file("unsaved");
  let session_file =
    fun
    | "" => Error("Session name cannot be empty.")
    | n => dir_file(n ++ ".ml");

  let session_of_path = p =>
    if (!Filename.check_suffix(p, ".ml")) {
      None;
    } else {
      Some((Filename.(basename(chop_extension(p))), p));
    };

  let sessions_of_dir = dir =>
    Result.bind(Dir.exists(dir)) @@
    (
      fun
      | false => Ok([])
      | true =>
        Result.bind(Dir.contents(dir)) @@
        (
          paths => {
            let rec add_session = (acc, p) =>
              switch (session_of_path(p)) {
              | None => acc
              | Some((n, p)) => [(n, p), ...acc]
              };

            Ok(List.sort(compare, List.fold_left(add_session, [], paths)));
          }
        )
    );

  let last_session = () =>
    Result.bind(last_session_file()) @@
    (
      file =>
        Result.bind(File.exists(file)) @@
        (
          fun
          | false => Ok(None)
          | true =>
            Result.bind(File.read(file)) @@
            (
              n => {
                let name = String.trim(n);
                Result.bind(session_file(name)) @@
                (
                  file =>
                    Result.bind(File.exists(file)) @@
                    (
                      fun
                      | false => Ok(None)
                      | true => Ok(Some((name, file)))
                    )
                );
              }
            )
        )
    );

  let set_last_session = n =>
    Result.bind(last_session_file()) @@ (file => File.set_content(~file, n));

  let get_session = n => {
    let find_session = n =>
      switch (n) {
      | "" => last_session()
      | n => Result.bind(session_file(n)) @@ (f => Ok(Some((n, f))))
      };

    Result.bind(find_session(n)) @@
    (
      fun
      | None => Error("No existing last session found.")
      | Some((name, file)) => {
          log_on_error(~use=(), set_last_session(name));
          [@implicit_arity] Ok(name, file);
        }
    );
  };

  let get_existing_session = n =>
    Result.bind(get_session(n)) @@
    (
      ((n, file) as s) =>
        Result.bind(File.exists(file)) @@
        (
          fun
          | true => Ok(s)
          | false =>
            Error(
              Fmt.str(
                "No session %a found, see '%a'",
                pp_code,
                n,
                pp_code,
                "Down.Session.list ()",
              ),
            )
        )
    );

  let last_name = () =>
    log_on_error(~use=None) @@
    Result.bind(last_session()) @@
    (
      fun
      | None => Ok(None)
      | Some((n, _)) => Ok(Some(n))
    );

  let list = () => {
    let pp_last = (ppf, last) =>
      if (!last) {
        ();
      } else {
        Fmt.pf(
          ppf,
          "(%a) ",
          Fmt.tty(style_last_indicator, Fmt.string),
          "last",
        );
      };

    let pp_session = (~last, ppf, (n, p)) =>
      Fmt.pf(
        ppf,
        "@[<h>%a %a%a@]",
        pp_code,
        n,
        pp_last,
        last == n,
        pp_faint(),
        p,
      );

    let pp_session_list = (~last, ppf, ss) =>
      Fmt.pf(ppf, "  @[<v>@,%a@,@]", Fmt.list(pp_session(~last)), ss);

    let pp_none = (ppf, dir) =>
      Fmt.pf(ppf, "  @[<v>@,No session found in %a@,@]", pp_faint(), dir);

    log_on_error(~use=()) @@
    Result.bind(dir()) @@
    (
      dir =>
        Result.bind(last_session()) @@
        (
          last =>
            Result.bind(sessions_of_dir(dir)) @@
            (
              fun
              | [] => Ok(Fmt.pr("%a@.", pp_none, dir))
              | ss => {
                  let last =
                    switch (last) {
                    | None => ""
                    | Some((last, _)) => last
                    };
                  Ok(Fmt.pr("%a@.", pp_session_list(~last), ss));
                }
            )
        )
    );
  };

  let load = (~silent=?, n) =>
    log_on_error(~use=()) @@
    Result.bind(get_existing_session(n)) @@
    (
      ((n, file)) =>
        if (use_file(~silent?, file)) {
          Ok();
        } else {
          Error(
            Fmt.str(
              "Use '%a' to correct errors.",
              pp_code,
              "Down.Session.edit \"\"",
            ),
          );
        }
    );

  let edit = n =>
    log_on_error(~use=()) @@
    Result.bind(get_session(n)) @@
    (
      ((_, file)) =>
        Result.bind(File.exists(file)) @@
        (
          fun
          | true => Editor.edit_file(file)
          | false =>
            /* create path */
            Result.bind(File.set_content(~file, "")) @@
            (() => Editor.edit_file(file))
        )
    );

  let err_exists = n =>
    Fmt.str(
      "Session %a exists, specify '%a' to overwrite.",
      pp_code,
      n,
      pp_code,
      "~replace:true",
    );

  let of_file = (~replace=false, ~file, n) =>
    log_on_error(~use=()) @@
    Result.bind(get_session(n)) @@
    (
      ((n, session_file)) =>
        Result.bind(File.exists(session_file)) @@
        (
          fun
          | true when !replace => Error(err_exists(n))
          | true
          | false =>
            Result.bind(File.read(file)) @@
            (contents => File.set_content(~file=session_file, contents))
        )
    );

  let delete = n =>
    log_on_error(~use=()) @@
    Result.bind(get_existing_session(n)) @@
    (((_, f)) => File.delete(f));

  /* Recording. */

  let sep = "(**)";
  let to_string = phrases => Txt_entries.to_string(~sep, phrases);
  let of_string = s => Txt_entries.of_string(~sep, s);

  let recording: ref(bool) = (ref(false): ref(bool));
  let set_recording = v => recording := v;
  let recording = () => recording^;

  let record: ref(list(string)) = (ref([]): ref(list(string)));
  let set_record = phrases => record := List.rev(phrases);
  let rem_last_recorded = () =>
    switch (record^) {
    | [] => ()
    | [_, ...rs] => record := rs
    };

  let add_recorded = s =>
    switch (String.trim(s)) {
    | s when String.length(s) >= 5 && String.sub(s, 0, 5) == "#quit" => ()
    | s => record := [s, ...record^]
    };

  let add_if_recording = s =>
    if (recording()) {
      add_recorded(s);
    };
  let recorded = () => List.rev(record^);
  let record = () => {
    module Top = (val top^: TOP);
    Top.read_interactive_input^ === original_ocaml_readline^
      ? log_error("Sorry, recording needs Down's line edition.")  /* That could still not be down's readline but, unlikely */
      : (
        if (recording()) {
          rem_last_recorded();
        } else {
          set_recording(true);
        }
      );
  };

  let stop = () =>
    if (recording()) {
      rem_last_recorded();
      set_recording(false);
    };
  let revise = () => {
    if (recording()) {
      rem_last_recorded();
    };
    log_on_error(~use=()) @@
    {
      let s = to_string(recorded());
      Result.bind(Editor.edit_string(~ext=".ml", s)) @@
      (s => Ok(set_record(of_string(s))));
    };
  };

  let save = (~replace=false, n) => {
    stop();
    log_on_error(~use=()) @@
    Result.bind(get_session(n)) @@
    (
      ((n, file)) =>
        switch (recorded()) {
        | [] => Error("No phrase to save.")
        | ps =>
          Result.bind(File.exists(file)) @@
          (
            fun
            | true when !replace => Error(err_exists(n))
            | true
            | false =>
              Result.bind(File.set_content(~file, to_string(ps))) @@
              (() => Ok(set_record([])))
          )
        }
    );
  };

  let append = n => {
    stop();
    log_on_error(~use=()) @@
    Result.bind(get_session(n)) @@
    (
      ((_, file)) =>
        switch (recorded()) {
        | [] => Error("No phrase to append.")
        | ps =>
          Result.bind(File.exists(file)) @@
          (
            fun
            | false =>
              Result.bind(File.set_content(~file, to_string(ps))) @@
              (() => Ok(set_record([])))
            | true =>
              Result.bind(File.read(file)) @@
              (
                contents => {
                  let ps = of_string(contents) @ ps;
                  Result.bind(File.set_content(~file, to_string(ps))) @@
                  (() => Ok(set_record([])));
                }
              )
          )
        }
    );
  };

  /* The idea of the following is to avoid a dialog to confirm losing
     existing recorded phrases. Though similar to history, it may be
     confusing on parallel ocaml processes. */

  let load_unsaved_record = () =>
    log_on_error(~use=()) @@
    Result.bind(unsaved_file()) @@
    (
      file =>
        Result.bind(File.exists(file)) @@
        (
          fun
          | false => Ok()
          | true =>
            Result.bind(File.read(file)) @@
            (
              contents => {
                set_record(of_string(contents));
                File.set_content(~file, "");
              }
            )
        )
    );

  let save_unsaved_record = () =>
    log_on_error(~use=()) @@
    (
      switch (recorded()) {
      | [] => Ok()
      | ps =>
        Result.bind(unsaved_file()) @@
        (
          file =>
            Result.bind(File.exists(file)) @@
            (
              fun
              | false => File.set_content(~file, to_string(ps))
              | true =>
                Result.bind(File.read(file)) @@
                (
                  contents =>
                    /* Another toplevel process may have written meanwhile... */
                    File.set_content(
                      ~file,
                      to_string(of_string(contents) @ ps),
                    )
                )
            )
        )
      }
    );

  /* Stepping. */

  let steps: ref(array(string)) = (ref([||]): ref(array(string)));
  let step = ref(-1);

  let step_next = () =>
    switch (step^) {
    | s when s >= Array.length(steps^) - 1 => None
    | s =>
      incr(step);
      Some(steps^[step^]);
    };

  let step_prev = () =>
    switch (step^) {
    | s when s <= 0 => None
    | s =>
      decr(step);
      Some(steps^[step^]);
    };

  let steps = n =>
    log_on_error(~use=()) @@
    Result.bind(get_existing_session(n)) @@
    (
      ((_, file)) =>
        Result.bind(File.read(file)) @@
        (
          contents => {
            steps := Array.of_list(of_string(contents));
            step := (-1);
            Ok();
          }
        )
    );

  let next_step = () => ignore(step_next());
  let prev_step = () => ignore(step_prev());
};

/* Ocaml syntax munging */

module Ocaml = {
  let id_path_char =
    fun
    | '0'..'9'
    | 'A'..'Z'
    | 'a'..'z'
    | '.'
    | '\''
    | '_' => true
    | _ => false;

  let id_span = (s, ~start) => {
    let id_char = Txt.find_prev(~sat=id_path_char, s, ~start);
    if (!id_path_char(s.[id_char])) {
      None;
    } else {
      let keep_prev = Txt.keep_prev_len(~sat=id_path_char, s, ~start=id_char);
      let id_start = id_char - keep_prev + 1;
      let id_len = Txt.keep_next_len(~sat=id_path_char, s, ~start=id_start);
      let id_end = id_start + id_len - 1;
      if (id_end < id_start) {
        None;
      } else {
        Some(String.sub(s, id_start, id_end - id_start + 1));
      };
    };
  };
};

/* Prompting */

module Prompt = {
  module I = {
    type t = Tty.input;
    let compare = compare;
  };
  module Itrie = Trie.Make(I);
  type cmd = t => [ | `Eoi | `Kont | `Break]
  and t = {
    mutable tty_w: int,
    readc: unit => option(int),
    output: string => unit,
    has_answer: (Tty.input, t) => option(string),
    id_complete: string => result((string, list(string)), string),
    id_info: string => result(option((string, string, string)), string),
    mutable clipboard: string,
    mutable txt: Pstring.t,
    /* These are zero-based rows relat. to the prompt line for clearing. */
    mutable last_cr: int, /* last cursor render row */
    mutable last_max_r: int /* last max render row */,
  };

  let has_answer = (input, p) => {
    /* FIXME ocaml and utop are a bit weird. Try to sort that out.
       Notably is there does seem to be any good reason not to input
       successive ;; separated phrases, ocaml does that on .ml files.
       Cf. https://github.com/ocaml/ocaml/issues/8813 */
    let ends_with_semisemi = s => {
      let rec loop = (s, i) =>
        i < 0
          ? false
          : (
            if (Txt.is_white(s.[i])) {
              loop(s, i - 1);
            } else if (s.[i] != ';') {
              false;
            } else if (i == 0) {
              false;
            } else {
              s.[i - 1] == ';';
            }
          );

      loop(s, String.length(s) - 1);
    };

    switch (input) {
    | `Enter =>
      let txt = Pstring.txt(p.txt);
      if (ends_with_semisemi(txt)) {
        Some(String.trim(txt) ++ "\n");
      } else {
        /* trim is for a cleaner hist. */
        None;
      };
    | _ => None
    };
  };

  let create =
      (
        ~id_complete=w => [@implicit_arity] Ok(w, []),
        ~id_info=id => Ok(None),
        ~output=Tty.output,
        ~readc,
        (),
      ) => {
    tty_w: 80,
    readc,
    output,
    has_answer,
    id_complete,
    id_info,
    clipboard: "",
    txt: Pstring.empty,
    last_cr: 0,
    last_max_r: 0,
  };

  /* Rendering */

  let ding = p => p.output(Tty.ding);
  let newline = p => p.output(Tty.newline);
  let error = (p, fmt) => {
    let error = Tty.styled_str(Tty.cap, style_err, "Error");
    let k = msg => p.output(Printf.sprintf("\r\n%s: %s\r\n", error, msg));
    Format.kasprintf(k, fmt);
  };

  let clear_screen = p =>
    p.output(String.concat("", Tty.[clear_screen, cursor_origin]));

  let clear_ui = p => {
    /* Go from cursor to max row and back to prompt row clearing lines. */
    let rec go = acc =>
      fun
      | 0 => acc
      | up => go([Tty.clear_row, Tty.cursor_up(1), ...acc], up - 1);

    let down = p.last_max_r - p.last_cr
    and up = p.last_max_r;
    let acc = go([Tty.clear_row, Tty.cursor_down(down)], up);
    p.output(String.concat("", List.rev(["\r", ...acc])));
    p.last_cr = 0;
    p.last_max_r = 0;
  };

  let prompt = "# ";
  let margin = "  ";
  let nl_margin = "\r\n  ";
  let render_prompt = (~active) => {
    let style =
      switch (Session.recording()) {
      | false =>
        if (active) {
          style_prompt;
        } else {
          style_prompt_inactive();
        }
      | true =>
        if (active) {
          style_prompt_recording;
        } else {
          style_prompt_recording_inactive();
        }
      };

    Tty.styled_str(Tty.cap, style, prompt);
  };

  let render_ui = (~active=true, p) => {
    let tty_w = p.tty_w
    and margin_w = String.length(margin);
    let (max_r, (cr, cc, c_nl)) =
      Pstring.geometry(~tty_w, ~margin_w, p.txt);
    let add_line = (acc, l) => [nl_margin, l, ...acc];
    let acc = [render_prompt(~active)];
    let acc = List.fold_left(add_line, acc, Txt.lines(Pstring.txt(p.txt)));
    let acc = ["\r", ...List.tl(acc)] /* remove exceeding nl_margin */;
    let acc =
      if (c_nl) /* cursor wrapped */ {
        ["\n", ...acc];
      } else {
        acc;
      };
    let acc =
      active
        ? [Tty.cursor_forward(cc), Tty.cursor_up(max_r - cr), ...acc] : acc;

    let ui = String.concat("", List.rev(acc));
    clear_ui(p);
    p.output(ui);
    if (active) {
      p.last_cr = cr;
      p.last_max_r = max_r;
    };
  };

  let render_id_complete = (p, prefix, candidates) => {
    let render_candidate = (prefix, c) =>
      switch (String.length(c)) {
      | 0 => ""
      | blen =>
        let styled = (sty, s) => Tty.styled_str(Tty.cap, sty, s);
        c.[0] == ' '
          /* Hackish. E.g. we don't actually get candidates one per line
             in case of module types. */
          ? styled(style_id_complete_info(), c)
          : (
            switch (String.index(c, '\t')) {
            | exception Not_found => c /* should not happen but be robust */
            | tab =>
              let rst_start = tab + 1;
              let suf_start = String.length(prefix);
              let pre = String.sub(c, 0, suf_start);
              let suf = String.sub(c, suf_start, tab - suf_start);
              let rst = ":" ++ String.sub(c, rst_start, blen - rst_start);
              Printf.sprintf(
                "  %s%s%s",
                pre,
                styled(style_id_complete_suff, suf),
                styled(style_id_complete_info(), rst),
              );
            }
          );
      };

    let candidates = List.map(render_candidate(prefix), candidates);
    render_ui(~active=false, p);
    newline(p);
    p.output(String.concat(Tty.newline, candidates));
    newline(p);
  };

  let render_id_info = (p, id, typ, doc) => {
    let render_id = id => Tty.styled_str(Tty.cap, style_id_info_id, id);
    let render_type = t => Tty.styled_str(Tty.cap, style_id_info_type, t);
    let typ =
      switch (Txt.lines(typ)) {
      | [""]
      | [] => ""
      | [t, ...ts] =>
        Printf.sprintf(":%s") @@
        render_type(
          String.concat(Tty.newline) @@
          [t, ...List.map(Printf.sprintf("%s%s", margin), ts)],
        )
      };

    let acc = [Printf.sprintf("\r\n  %s%s", render_id(id), typ)];
    let acc = List.rev_append(Txt.lines(doc), acc);
    let acc = List.rev_map(Printf.sprintf("%s%s", margin), acc);
    render_ui(~active=false, p);
    newline(p);
    p.output(String.concat(Tty.newline, acc));
    newline(p);
  };

  /* Commands */

  let set_txt_value = (p, txt) => p.txt = Pstring.eoi(Pstring.v(txt));
  let set_txt_with_history = (op, p) =>
    switch (op(Pstring.txt(p.txt))) {
    | None => ding(p)
    | Some(txt) => set_txt_value(p, txt)
    };

  let update = (op, p) => {
    let txt = op(p.txt);
    if (txt === p.txt) /* bof */ {
      ding(p);
    } else {
      p.txt = txt;
    };
  };

  let update_with_kill = (op, p) => {
    let (txt, kill) = op(p.txt);
    let clipboard =
      switch (kill) {
      | None =>
        ding(p);
        p.clipboard;
      | Some(s) => s
      };
    p.txt = txt;
    p.clipboard = clipboard;
  };

  let soi = update(Pstring.soi);
  let eoi = update(Pstring.eoi);
  let sol = update(Pstring.sol);
  let eol = update(Pstring.eol);
  let prev_char = update(Pstring.prev_char);
  let next_char = update(Pstring.next_char);
  let prev_word = update(Pstring.prev_word);
  let next_word = update(Pstring.next_word);
  let prev_line = update(Pstring.prev_line);
  let next_line = update(Pstring.next_line);
  let insert = bytes => update(Pstring.insert(bytes));
  let delete_next_char = update(Pstring.delete_next_char);
  let delete_prev_char = update(Pstring.delete_prev_char);
  let set_mark = update(p => Pstring.set_mark(Some(Pstring.cursor(p)), p));
  let swap_cursor_and_mark = update(Pstring.swap_cursor_and_mark);
  let kill_prev_word = update_with_kill(Pstring.kill_prev_word);
  let kill_next_word = update_with_kill(Pstring.kill_next_word);
  let kill_to_sol = update_with_kill(Pstring.kill_to_sol);
  let kill_to_eol = update_with_kill(Pstring.kill_to_eol);
  let kill_region = update_with_kill(Pstring.kill_region);
  let yank = p =>
    if (p.clipboard == "") {
      ding(p);
    } else {
      let mark = Some(Pstring.cursor(p.txt))
      and clip = p.clipboard;
      update(p => Pstring.set_mark(mark, Pstring.insert(clip, p)), p);
    };

  let edit = p =>
    switch (Editor.edit_string(~ext=".ml", Pstring.txt(p.txt))) {
    | Error(e) => error(p, "%s", e)
    | Ok(txt) => set_txt_value(p, txt)
    };

  let id_complete = p => {
    let completion_start = p => {
      let rec loop = (s, i) =>
        if (i >= 0 && Ocaml.id_path_char(s.[i])) {
          loop(s, i - 1);
        } else {
          let ret = i + 1;
          if (ret == Pstring.cursor(p)) {
            None;
          } else {
            Some(ret);
          };
        };

      loop(Pstring.txt(p), Pstring.cursor(p) - 1);
    };

    switch (completion_start(p.txt)) {
    | None => ding(p)
    | Some(start) =>
      let set_subst = (p, start, old, w) =>
        p.txt = Pstring.subst(start, start + String.length(old), w, p.txt);

      let word = Pstring.txt_range(start, Pstring.cursor(p.txt) - 1, p.txt);
      switch (p.id_complete(word)) {
      | Error(e) => error(p, "%s", e)
      | [@implicit_arity] Ok(_, []) => ding(p)
      | [@implicit_arity] Ok(w, [_]) => set_subst(p, start, word, w)
      | [@implicit_arity] Ok(w, cs) =>
        render_id_complete(p, w, cs);
        set_subst(p, start, word, w);
      };
    };
  };

  let id_info = p =>
    switch (Ocaml.id_span(Pstring.txt(p.txt), ~start=Pstring.cursor(p.txt))) {
    | None => ding(p)
    | Some(id) =>
      switch (p.id_info(id)) {
      | Error(e) => error(p, "%s", e)
      | Ok(None) => ding(p)
      | Ok(Some((id, typ, doc))) => render_id_info(p, id, typ, doc)
      }
    };

  let ctrl_d = p =>
    if (Pstring.txt(p.txt) == "") {
      `Eoi;
    } else {
      delete_next_char(p);
      `Kont;
    };

  let session_next_step = p =>
    switch (Session.step_next()) {
    | None => ding(p)
    | Some(s) => set_txt_value(p, s)
    };

  let session_prev_step = p =>
    switch (Session.step_prev()) {
    | None => ding(p)
    | Some(s) => set_txt_value(p, s)
    };

  let prev_history = set_txt_with_history(History.prev);
  let next_history = set_txt_with_history(History.next);
  let break = p => `Break;
  let kont = (f, p) => {
    f(p);
    `Kont;
  };
  let cmds: list((list(Tty.input), cmd, string)) = (
    [
      ([`Home], kont(soi), "move to start of input"),
      ([`End], kont(eoi), "move to end of input"),
      ([`Ctrl(`Key(0x61)) /* a */], kont(sol), "move to start of line"),
      ([`Ctrl(`Key(0x65)) /* e */], kont(eol), "move to end of line"),
      (
        [`Ctrl(`Key(0x62)) /* b */],
        kont(prev_char),
        "move to previous character",
      ),
      (
        [`Ctrl(`Key(0x66)) /* f */],
        kont(next_char),
        "move to next character",
      ),
      ([`Arrow(`Left)], kont(prev_char), "move to previous character"),
      ([`Arrow(`Right)], kont(next_char), "move to next character"),
      (
        [`Meta(0x62) /* b */],
        kont(prev_word),
        "move to start of previous word",
      ),
      (
        [`Meta(0x66) /* f */],
        kont(next_word),
        "move after the end of next word",
      ),
      (
        [`Ctrl(`Arrow(`Left))],
        kont(prev_word),
        "move to start of previous word",
      ),
      (
        [`Ctrl(`Arrow(`Right))],
        kont(next_word),
        "move after the end of next word",
      ),
      (
        [`Ctrl(`Key(0x70)) /* p */],
        kont(prev_line),
        "move to previous line",
      ),
      ([`Ctrl(`Key(0x6E)) /* n */], kont(next_line), "move to next line"),
      ([`Arrow(`Up)], kont(prev_history), "previous history entry"),
      ([`Arrow(`Down)], kont(next_history), "next history entry"),
      ([`Backspace], kont(delete_prev_char), "delete previous character"),
      ([`Delete], kont(delete_next_char), "delete next character"),
      (
        [`Ctrl(`Key(0x64)) /* d */],
        ctrl_d,
        "delete next character or exit if input is empty",
      ),
      ([`Ctrl(`Key(0x63)) /* c */], break, "abandon input"),
      ([`Ctrl(`Key(0x60)) /* space ? */], kont(set_mark), "set the mark"),
      (
        [`Ctrl(`Key(0x78)) /* x */, `Ctrl(`Key(0x78)) /* x */],
        kont(swap_cursor_and_mark),
        "swap cursor and mark",
      ),
      ([`Ctrl(`Key(0x79)) /* y */], kont(yank), "yank"),
      (
        [`Ctrl(`Key(0x6B)) /* k */],
        kont(kill_to_eol),
        "kill to end of line",
      ),
      (
        [`Ctrl(`Key(0x75)) /* k */],
        kont(kill_to_sol),
        "kill to start of line",
      ),
      (
        [`Meta(0x7F)],
        kont(kill_prev_word),
        "kill to start of previous word",
      ),
      (
        [`Meta(0x64) /* d */],
        kont(kill_next_word),
        "kill to end of next word",
      ),
      ([`Ctrl(`Key(0x77)) /* w */], kont(kill_region), "kill region"),
      (
        [`Shift(`Arrow(`Up))],
        kont(session_prev_step),
        "previous session step",
      ),
      (
        [`Shift(`Arrow(`Down))],
        kont(session_next_step),
        "next session step",
      ),
      (
        [`Ctrl(`Key(0x78)) /* x */, `Ctrl(`Key(0x70)) /* p */],
        kont(session_prev_step),
        "previous session step",
      ),
      (
        [`Ctrl(`Key(0x78)) /* x */, `Ctrl(`Key(0x6E)) /* n */],
        kont(session_next_step),
        "next session step",
      ),
      ([`Ctrl(`Key(0x6C)) /* l */], kont(clear_screen), "clear screen"),
      ([`Tab], kont(id_complete), "complete identifier"),
      (
        [`Ctrl(`Key(0x74)) /* t */],
        kont(id_info),
        "show identifier type and documentation",
      ),
      (
        [`Ctrl(`Key(0x78)) /* x */, `Ctrl(`Key(0x65)) /* e */],
        kont(edit),
        "edit input with external program (VISUAL or EDITOR env var)",
      ),
    ]:
      list((list(Tty.input), cmd, string))
    /***/
    /***/
    /***/
    /***/
    /***/
    /***/
    /***/
    /***/
    /***/
    /***/
    /***/
  );

  let pp_cmd = (ppf, (is, _, doc)) => {
    let pp_is = Fmt.tty(style_key, Fmt.(list(~sep=sp, Tty.pp_input)));
    Fmt.pf(ppf, "@[%a  @[%a@]@]", pp_is, is, Fmt.text, doc);
  };

  let cmd_trie = {
    let add = (t, (is, cmd, _doc)) => Itrie.add(is, Some(cmd), t);
    List.fold_left(add, Itrie.empty, cmds);
  };

  /* Event loop */

  let ask = p => {
    let reset = p => {
      p.last_cr = 0;
      p.last_max_r = 0;
      p.txt = Pstring.empty;
    };
    let resize = p => p.tty_w = Tty.width(p.readc);
    let return = p => {
      render_ui(~active=false, p);
      newline(p);
    };
    let rec loop = (p, input_state) => {
      render_ui(p);
      switch (Tty.input(p.readc)) {
      | None =>
        /* EINTR (and thus SIGWINCH) */ resize(p);
        loop(p, input_state);
      | Some(i) =>
        switch (p.has_answer(i, p)) {
        | Some(a) =>
          History.add(a);
          Session.add_if_recording(a);
          return(p);
          `Answer(a);
        | None =>
          let input_state = Itrie.find([i], input_state);
          switch (Itrie.value(input_state)) {
          | None when Itrie.is_empty(input_state) =>
            switch (i) {
            | `Bytes(bytes) =>
              insert(bytes, p);
              loop(p, cmd_trie);
            | `Enter =>
              insert("\n", p);
              loop(p, cmd_trie);
            | _ =>
              ding(p);
              loop(p, cmd_trie);
            }
          | None => loop(p, input_state)
          | Some(cmd) =>
            switch (cmd(p)) {
            | `Kont => loop(p, cmd_trie)
            | `Break =>
              History.restart();
              return(p);
              `Break;
            | `Eoi => `Eoi
            }
          };
        }
      };
    };

    reset(p);
    resize(p);
    loop(p, cmd_trie);
  };
};

/* Help */

let help = () => {
  let pp_manual = (ppf, ()) =>
    Fmt.pf(
      ppf,
      "@[Consult '%a' for the manual and API.@]",
      pp_code,
      "odig doc down",
    );

  let pp_session = (ppf, ()) =>
    Fmt.pf(
      ppf,
      "%a:@,Support for sessions is in the %a module.@,Use '%a' to list sessions.",
      pp_doc_section,
      "Sessions",
      pp_code,
      "Down.Session",
      pp_code,
      "Down.Session.list ()",
    );

  let pp_key_bindings = (ppf, ()) =>
    Fmt.pf(
      ppf,
      "%a:@,@[<v>%a@]",
      pp_doc_section,
      "Key bindings",
      Fmt.list(Prompt.pp_cmd),
      Prompt.cmds,
    );

  let pp_help = (ppf, ()) =>
    Fmt.pf(
      ppf,
      "  @[<v>@,%a@,%a@,@,%a@,@,%a@,@]@.",
      pp_doc_section,
      "Welcome to Down!",
      pp_manual,
      (),
      pp_session,
      (),
      pp_key_bindings,
      (),
    );

  pp_help(Format.std_formatter, ());
};

/* Completion and doc lookup via ocp-index */

module Ocp_index = {
  /* FIXME. POC hack via ocp-index, we likely want to that ourselves since we
     also need to peek in the OCaml toplevel symtable to be able to
     complete what the user defined and keep track of [open]s. */

  module Ctrie = Trie.Make(Char);

  let string_to_list = s => {
    let rec go = (s, i, acc) =>
      if (i < 0) {
        acc;
      } else {
        go(s, i - 1, [s.[i], ...acc]);
      };
    go(s, String.length(s) - 1, []);
  };

  let string_of_list = l => {
    let b = Buffer.create(255);
    List.iter(Buffer.add_char(b), l);
    Buffer.contents(b);
  };

  let has_ocp_index =
    lazy(
      switch (Cmd.exists(["ocp-index"])) {
      | Error(_) as e => e
      | Ok(true) => Ok()
      | Ok(false) =>
        Error(
          Fmt.str(
            "Completion and doc lookup needs ocp-index. Try '%a'.",
            pp_code,
            "opam install ocp-index",
          ),
        )
      }
    );

  let complete_cmd = token => [
    "ocp-index",
    "complete",
    "-f",
    "%q \t %t",
    token,
  ];
  let complete_word = (word, results) => {
    let add_id = (acc, r) => {
      let id = String.trim(List.hd(String.split_on_char('\t', r)));
      if (id == "") {
        acc;
      } else {
        Ctrie.add(string_to_list(id), Some(), acc);
      };
    };

    let ids = List.fold_left(add_id, Ctrie.empty, results);
    let (word, _) = Ctrie.find_fork(string_to_list(word), ids);
    (string_of_list(word), results);
  };

  let finish_single_complete =
    fun
    | "" => ""
    | w when w.[String.length(w) - 1] == '.' => w
    | w => {
        let path_start =
          switch (String.rindex(w, '.')) {
          | exception Not_found => 0
          | i => i + 1
          };

        if (Char.uppercase_ascii(w.[path_start]) == w.[path_start]) {
          w ++ ".";
        } else {
          /* Likely Module name path segment. */
          w ++ " ";
        };
      }; /* Likely Module structure item segment. */

  let id_complete =
    fun
    | "" => [@implicit_arity] Ok("", [])
    | w =>
      Result.bind(Lazy.force(has_ocp_index)) @@
      (
        () =>
          Result.bind(Result.map_error(snd) @@ Cmd.read(complete_cmd(w))) @@
          (
            s =>
              switch (List.rev(Txt.lines(s))) {
              | []
              | [""]
              | ["", ""] => [@implicit_arity] Ok(w, [])
              | ["", ...rlines]
              | rlines =>
                switch (complete_word(w, List.rev(rlines))) {
                | (w, [_] as cs) =>
                  [@implicit_arity] Ok(finish_single_complete(w), cs)
                | _ as ret => Ok(ret)
                }
              }
          )
      );

  let print_cmd = id => ["ocp-index", "print", id, "%q \\t %t\\n(**)\\n%d"];
  let parse_id_info =
    fun
    | "" => None
    | o =>
      switch (Txt_entries.of_string(~sep="(**)", o)) {
      | [] => None
      | [v] => Some((v, "", ""))
      | [id_sig, doc, ..._] =>
        switch (String.index(id_sig, '\t')) {
        | exception Not_found => Some((o, "", doc))
        | i =>
          let len = String.length(id_sig);
          let id = String.sub(id_sig, 0, i);
          let typ =
            if (i + 1 == len) {
              "";
            } else {
              String.sub(id_sig, i + 1, len - (i + 1));
            };

          Some((id, typ, doc));
        }
      };

  let id_info =
    fun
    | "" => Ok(None)
    | id =>
      Result.bind(Lazy.force(has_ocp_index)) @@
      (
        () =>
          switch (Cmd.read(print_cmd(id))) {
          | [@implicit_arity] Error(2, _) => Ok(None)
          | [@implicit_arity] Error(n, e) => Error(e)
          | Ok(o) => Ok(parse_id_info(o))
          }
      );
};

/* Toplevel readline */

let blit_toploop_buf = (s, i, b, blen) => {
  let slen = String.length(s);
  let slen_to_write = slen - i;
  let len = min(blen, slen_to_write);
  let snext = i + len;
  Bytes.blit_string(s, i, b, 0, len);
  (
    len,
    if (snext < slen) {
      Some((s, snext));
    } else {
      None;
    },
  );
};

let down_readline = p => {
  let rem = ref(None);
  (prompt, b, len) =>
    switch (rem^) {
    | Some((s, i)) =>
      let (len, rem') = blit_toploop_buf(s, i, b, len);
      rem := rem';
      (len, false);
    | None =>
      let rec loop = p =>
        switch (Prompt.ask(p)) {
        | `Eoi => (0, true)
        | `Break =>
          Tty.output("Interrupted.\r\n");
          loop(p);
        | `Answer(ans) =>
          let (len, rem') = blit_toploop_buf(ans, 0, b, len);
          rem := rem';
          (len, false);
        };

      switch (Stdin.set_raw_mode(true)) {
      | false => original_ocaml_readline^(prompt, b, len)
      | true =>
        let r = loop(p);
        ignore(Stdin.set_raw_mode(false));
        r;
      };
    };
};

external sigwinch: unit => int = "ocaml_down_sigwinch";
let install_sigwinch_interrupt = () =>
  /* Sufficient to interrupt the event loop on window size changes. */
  Sys.set_signal(sigwinch(), Sys.Signal_handle(_ => ()));

let pp_announce = (ppf, ()) =>
  Fmt.pf(
    ppf,
    "%a v0.0.2 loaded. Type %a for more info.",
    pp_doc_section,
    "Down",
    pp_code,
    "Down.help ()",
  );

let err_no_ansi = "no ANSI terminal capability detected.";
let err_no_raw = "failed to set stdin in raw mode.";
let install_down = () => {
  let line_edition =
    switch (Tty.cap) {
    | `None => Error(err_no_ansi)
    | `Ansi =>
      switch (Stdin.set_raw_mode(true)) {
      | false => Error(err_no_raw)
      | true =>
        ignore(Stdin.set_raw_mode(false));
        Ok();
      }
    };

  let announce = () => Fmt.pr("%a@.", pp_announce, ());
  module Top = (val top^: TOP);
  History.load();
  at_exit(History.save);
  Session.load_unsaved_record();
  at_exit(Session.save_unsaved_record);
  original_ocaml_readline := Top.read_interactive_input^;
  switch (line_edition) {
  | Ok () =>
    let id_complete = Ocp_index.id_complete;
    let id_info = Ocp_index.id_info;
    let p = Prompt.create(~id_complete, ~id_info, ~readc=Stdin.readc, ());
    Top.read_interactive_input := down_readline(p);
    install_sigwinch_interrupt();
    announce();
  | Error(err) =>
    announce();
    log_warning("Down line edition disabled: %s", err);
  };
};

/* Private */

module Private = {
  module type TOP = TOP;
  let set_top = t => {
    top := t;
    install_down();
  };

  let unicode_version = Down_tty_width.unicode_version;

  let tty_test = () =>
    switch (Tty.cap) {
    | `None => print_endline(err_no_ansi)
    | `Ansi =>
      switch (Stdin.set_raw_mode(true)) {
      | false => print_endline(err_no_raw)
      | true =>
        let w = Tty.width(Stdin.readc);
        let welcome =
          Fmt.str(
            "\r\nWelcome to Down's TTY test! Your width is %d. Ding!\r\nHit your keyboard. C-{c,d} stops the test.\r\n\r\n",
            w,
          );

        Tty.output(welcome);
        Tty.output(Tty.ding);
        let rec loop = () =>
          switch (Tty.input(Stdin.readc)) {
          | None =>
            let w = Tty.width(Stdin.readc);
            Tty.output(Fmt.str("Your width is %d.\r\n", w));
            loop();
          | Some(i) =>
            switch (i) {
            | `Ctrl(`Key(0x63)) /* c */ => print_endline("Bye.\r")
            | `Ctrl(`Key(0x64)) /* d */ => print_endline("EOF Bye.\r")
            | _ =>
              print_endline(Format.asprintf("%a\r", Tty.pp_input, i));
              loop();
            }
          };

        loop();
      }
    };
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
