/*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   down v0.0.2
  ---------------------------------------------------------------------------*/

/** An OCaml toplevel (REPL) upgrade

    See the {{!page-manual}manual}. */;

/** {1 Down} */;

/** [help ()] prints help about Down. */

let help: unit => unit;

/** Manage sessions.

    See the {{!page-manual.sessions}manual}. */

module Session: {
  /** {1:sessions Sessions} */;

  /** The type for session names. Use [""] to denote the {!last_name}
      session. */

  type name = string;

  /** [last_name ()] is the last session name that was used with any
      of the functions of this module; if any and still
      existing. Persisted accross [ocaml] invocations. */

  let last_name: unit => option(string);

  /** [list ()] lists available sessions. */

  let list: unit => unit;

  /** [load s] loads and executes session [s]. If [silent] is [true]
      the result of phrases is not printed out (defaults to
      [false]). */

  let load: (~silent: bool=?, name) => unit;

  /** [edit s] edits session [s] in your editor. A session is created
      if [s] does not exist. */

  let edit: name => unit;

  /** [of_file ~replace ~file s] takes the contents of file [file] and
      stores it session [s]. The function errors if [s] exists; unless
      [replace] is [true] (defaults to [false]). */

  let of_file: (~replace: bool=?, ~file: string, name) => unit;

  /** [delete s] deletes session [s]. */

  let delete: name => unit;

  /** {1:record Recording}

      {b Note.} Unsaved recorded phrases are persisted across [ocaml]
      sessions. */;

  /** [record] starts recording phrases. */

  let record: unit => unit;

  /** [stop] stops recording phrases. */

  let stop: unit => unit;

  /** [revise ()] edits recorded phrases. */

  let revise: unit => unit;

  /** [save s] saves recorded phrases to [s], stops recording and
      clears the recorded phrases. The function errors and the
      recorded phrases are kept intact if [s] exists; unless [replace]
      is [true] (defaults to [false]). See also {!append}. */

  let save: (~replace: bool=?, name) => unit;

  /** [append s] is like {!save} except it appends to [s] or creates
      it if it does not exist. */

  let append: name => unit;

  /** {1:stepping Stepping} */;

  /** [steps ()] loads a session for stepping through manually via
      [shift-{up,down}] (or [C-x C-{p,n}]). */

  let steps: name => unit;

  /** [next_step ()] moves to the next step of the stepped session.
      Usually you do this via [shift-down] or [C-x C-n]. */

  let next_step: unit => unit;

  /** [prev_step ()] moves the previous step of the stepped session.
      Usually you do this via [shift-up] or [C-x C-p]. */

  let prev_step: unit => unit;
};

/** Manage history. */

module History: {
  /** [edit ()] edits history in your editor. */

  let edit: unit => unit;

  /** [clear ()] clears the history. */

  let clear: unit => unit;
};

/** [tty_no_faint ()] disables uses of ANSI faint by down.  Some
    terminals switch to different colors which may be unpleasant, call
    this function if that is the case. */

let tty_no_faint: unit => unit;

/** Private.

    Do not use. This is an unstable API subject to change even between
    minor versions of the library. */

module Private: {
  /** OCaml Toplevel API */

  module type TOP = {
    let read_interactive_input: ref((string, bytes, int) => (int, bool));
    let use_file: (Format.formatter, string) => bool;
    let use_silently: (Format.formatter, string) => bool;
  };

  /** [set_top t] sets the implementation of the OCaml toplevel to [t]. */

  let set_top: (module TOP) => unit;

  /** [unicode_version] is the Unicode version on which the
      {!Uucp.Break.tty_width_hint} data used by down is based. */

  let unicode_version: string;

  /** [tty_test ()] interactively tests how terminal input is parsed by
      down. */

  let tty_test: unit => unit;
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
