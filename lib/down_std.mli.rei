/*---------------------------------------------------------------------------
   Copyright (c) 2017 The down programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   down v0.0.2
  ---------------------------------------------------------------------------*/

/** Down standard needs. */;

/** Result values. */

module Result: {
  let map_error: ('e => 'f, result('a, 'e)) => result('a, 'f);
  let bind: (result('a, 'b), 'a => result('c, 'b)) => result('c, 'b);
};

/** Simple tries with incremental lookup. */

module Trie: {
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
  module Make: (T: Map.OrderedType) => S with type elt = T.t;
};

/** UTF-8 text handling, possibly malformed.

    {b Note.} [start], [after] and [before] arguments can be out of
    bounds and in particular equal to the string length.  Finding
    forwards returns the string length if it cannot be found, finding
    backwards returns 0 if it cannot be found. */

module Txt: {
  /** [find_next ~sat s ~start] is either the [Sys.max_string s] or
      the index of the byte at or after [start] that satisfies [sat]. */

  let find_next: (~sat: char => bool, string, ~start: int) => int;

  /** [find_prev ~sat s ~start] is either the [0] or
      the index of the byte at or before [start] that satisfies [sat]. */

  let find_prev: (~sat: char => bool, string, ~start: int) => int;

  /** [keep_next_len ~sat s ~start] is the number of consecutive
      next [sat] satisfying bytes starting at [start], included. */

  let keep_next_len: (~sat: char => bool, string, ~start: int) => int;

  /** [keep_prev_len ~sat s ~start] is the number of consecutive
      previous [sat] satisfying bytes starting at [start], included. */

  let keep_prev_len: (~sat: char => bool, string, ~start: int) => int;

  /** {1:lines Lines} */;

  /** [lines s] splits [s] into CR, CRLF, LF lines separated lines. This is
      [[""]] on the empty string. */

  let lines: string => list(string);

  /** [is_eol] is [true] iff [c] is ['\r'] or ['\n']. */

  let is_eol: char => bool;

  /** [find_next_eol s ~start] is either [Sys.max_string s] or the index of
      the byte at or after [start] that satisfies {!is_eol}. */

  let find_next_eol: (string, ~start: int) => int;

  /** [find_prev_eol s ~start] is either [0] or the index of the byte
      at or before [start] that satisfies {!is_eol}. */

  let find_prev_eol: (string, ~start: int) => int;

  /** [find_prev_sol s ~start] is either [0] or the position {e after} the
      byte at or before [start] that satisfies {!is_eol}. This can be
      [Sys.max_string s]. */

  let find_prev_sol: (string, ~start: int) => int;

  /** {1:uchar UTF-8 encoded Unicode characters} */;

  /** [utf_8_decode_len b] is the length of an UTF-8 encoded Unicode
      character starting with byte [b]. This is [1] on UTF-8
      continuation or malformed bytes. */

  let utf_8_decode_len: char => int;

  /** [is_utf_8_decode c] is [true] iff [c] is not an UTF-8 continuation
      byte. This means [c] is either an UTF-8 start byte or an UTF-8
      malformed byte. */

  let is_utf_8_decode: char => bool;

  /** [find_next_utf_8_sync s ~start] is either [Sys.max_string s] or the
      index of the byte at or after [start] that satisfies
      {!is_utf_8_decode}. */

  let find_next_utf_8_decode: (string, ~start: int) => int;

  /** [find_prev_utf_8_decode s ~start] is either [0] or the index of the
      byte at or before [start] that satisfies {!is_utf_8_decode}. */

  let find_prev_utf_8_decode: (string, ~start: int) => int;

  /** {1:white Whitespace} */;

  /** [is_white c] is [true] iff [c] is US-ASCII whitespace (0x20,
      0x09, 0x0A, 0x0B, 0x0C or 0x0D). */

  let is_white: char => bool;

  /** [find_next_white s ~start] is either [String.length s] or the first
      byte position at or after [start] such that {!is_white} is
      [true]. */

  let find_next_white: (string, ~start: int) => int;

  /** [find_prev_white s ~start] is either either [0] or the first byte
      position at or before [start] such that {!is_white} is
      [true]. */

  let find_prev_white: (string, ~start: int) => int;

  /** {1:words Words} */;

  /** [find_next_after_eow] is either [String.length s] or the byte position
      of the first {!is_white} after first skipping white and then
      non-white starting at [start]. */

  let find_next_after_eow: (string, ~start: int) => int;

  /** [find_prev_sow] is either [0] or the byte position after skipping
      backward first white and then non-white. */

  let find_prev_sow: (string, ~start: int) => int;

  /** {1:gc Grapheme clusters and TTY width}

      {b Note.} This is a simple notion of grapheme cluster based
      on {!Uucp.Break.tty_width_hint}. */;

  /** [find_next_gc s ~after] is [String.length s] or the byte position of
      the grapheme cluster after the one starting at [after]. */

  let find_next_gc: (string, ~after: int) => int;

  /** [find_next_gc_and_width s ~after] is like {!find_next_gc} but
      also returns in the second component the tty width of the
      grapheme cluster at [after]. */

  let find_next_gc_and_tty_width: (string, ~after: int) => (int, int);

  /** [find_prev_gc s ~before] is [0] or the the byte position of the
      grapheme cluster before the one starting at [before]. */

  let find_prev_gc: (string, ~before: int) => int;

  /** [find_prev_eol_and_tty_width s ~before] is either [0] or the
      index of the byte before [before] that satisfies {!is_eol} and
      in the second component, the tty width needed to go from that index
      to [before]. */

  let find_prev_eol_and_tty_width: (string, ~before: int) => (int, int);

  /** [find_next_tty_width_or_eol s ~start ~w] is the index of the grapheme
      cluster after TTY width [w] at or after [start] or of the next
      end of line if that happened before. */

  let find_next_tty_width_or_eol: (string, ~start: int, ~w: int) => int;
};

/** Text entries parsing.

    Parsing text made of entries separated by a special line. */

module Txt_entries: {
  /** [to_string ~sep es] converts entries [es] to a string
      by concatening them and separating them by lines containing
      [sep]. */

  let to_string: (~sep: string, list(string)) => string;

  /** [of_string ~sep s] are the entries of [s]. Entries
      are separated by trimmed lines that contain [sep]. */

  let of_string: (~sep: string, string) => list(string);
};

/** Environment variables. */

module Env: {
  /** [get var] is environment variable [var]. If the variable
      is empty [None] is returned. */

  let get: string => option(string);
};

/** Directories */

module Dir: {
  /** [config ()] is the directory used to store user-specific program
      configuration. */

  let config: unit => result(string, string);

  /** [create dir] creates directory [dir]. */

  let create: string => result(unit, string);

  /** [exists dir] is [true] if [dir] exists as a directory. */

  let exists: string => result(bool, string);

  /** [contents dir] is the directory contents of [dir]. */

  let contents: string => result(list(string), string);
};

/** Files. */

module File: {
  /** [null] is an empty file that discards all writes. */

  let null: string;

  /** [exists file] checks for file (or directory) existence. */

  let exists: string => result(bool, string);

  /** [delete file] deletes file [file]. */

  let delete: string => result(unit, string);

  /** [rename src ~dst] renames [src] into [dst]. */

  let rename: (string, ~dst: string) => result(unit, string);

  /** [read file] is the contents of file [file]. */

  let read: string => result(string, string);

  /** [write file d] writes [d] to [file]. */

  let write: (~file: string, string) => result(unit, string);

  /** [set_content ~file s] sets the contents of file [file] to [s].
      The path to the file is created if it doesn't exist. */

  let set_content: (~file: string, string) => result(unit, string);

  /** [tmp ()] is a temporary file whose name ends with [suff]. */

  let tmp: (~suff: string=?, unit) => result(string, string);
};

/** Executing commands */

module Cmd: {
  /** The type for commands. */

  type t = list(string);

  /** [of_string s] is a command from string [s]. */

  let of_string: string => t;

  /** [exists cmd] checks if the tool in [cmd] exists. */

  let exists: t => result(bool, string);

  /** [must_exist cmd] checks the tool in [cmd] exists and fails
      with an error message otherwise. */

  let must_exist: t => result(t, string);

  /** [run ~stdout ~stderr c] run commands [c] and returns an error on non zero
      exit. [stdout] and [stderr] can be used to redirect the corresponding
      outputs to files. */

  let run:
    (~stdout: string=?, ~stderr: string=?, t) => result(unit, (int, string));

  /** [read c] runs commands [c] and returns its standard input
      or non zero on exit. */

  let read: t => result(string, (int, string));
};

/** Terminal interaction. */

module Tty: {
  /** {1:cap Capabilities} */;

  /** The type for capabilities. */

  type cap = [ | `None | `Ansi];

  /** [cap] is the current terminal capability. This
        only uses environment variables to detect it. */

  let cap: cap;

  /** {1:ansi ANSI Styling} */;

  /** The type for ANSI colors. */

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

  /** The type for ANSI styles. */

  type style = [
    | `Bold
    | `Faint
    | `Italic
    | `Underline
    | `Reverse
    | `Fg([ color | `Hi(color)])
    | `Bg([ color | `Hi(color)])
  ];

  /** [styled_str cap styles s] is [s] styled according to [cap] and
      [styles]. */

  let styled_str: (cap, list(style), string) => string;

  /** {1:out Output} */;

  /** [output s] outputs [s] on [stdout] and flushes it. */

  let output: string => unit;

  /** [ding] rings the bell. */

  let ding: string;

  /** [newline] is CRLF. */

  let newline: string;

  /** [clear_row] erases the row. */

  let clear_row: string;

  /** [cursor_up n] moves up [n] rows. */

  let cursor_up: int => string;

  /** [cursor_down n] moves down [n] rowns. */

  let cursor_down: int => string;

  /** [cursor_forward n] moves cursor by [n] columns. */

  let cursor_forward: int => string;

  /** [cursor_origin] moves cursor to the top-left origin. */

  let cursor_origin: string;

  /** [clear_screen] clears the screen. */

  let clear_screen: string;

  /** {1:input Input} */;

  type arrow = [ | `Up | `Down | `Left | `Right];
  /** The type for user input. */

  type input = [
    | `Arrow(arrow)
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

  /** [input readc] is user input read byte-by-byte using [readc]. */

  let input: (unit => option(int)) => option(input);

  /** [pp_input] formats inputs. */

  let pp_input: (Format.formatter, input) => unit;

  /** {1:witdh Width} */;

  /** [width readc] tries to termine the tty width using {!output} and
      [readc] to read the result. */

  let width: (unit => option(int)) => int;
};

/** Setup standard input for tty interaction.

    {b Note.} When the module is loaded an {!at_exit} handler
    is installed to make sure [set_raw_mode false] is called
    at the end of the program. */

module Stdin: {
  /** {1:raw Raw mode and character input} */;

  /** [set_raw_mode raw] sets stdin raw mode according to [raw]
        returns [false] if the operation fails. */

  let set_raw_mode: bool => bool;

  /** [readc ()] reads a single byte from [stdin]. This is a
        blocking [read]. [None] is returned in case the operation
        failed for some reason. */

  let readc: unit => option(int);
};

/** Format module helpers. */

module Fmt: {
  /** The type for value formatters. */

  type t('a) = (Format.formatter, 'a) => unit;

  /** [pf] is {!Format.fprintf} */

  let pf: (Format.formatter, format('a, Format.formatter, unit)) => 'a;

  /** [pf] is {!Format.printf} */

  let pr: format('a, Format.formatter, unit) => 'a;

  /** [kpf] is {!Format.kfprintf}. */

  let kpf:
    (
      Format.formatter => 'a,
      Format.formatter,
      format4('b, Format.formatter, unit, 'a)
    ) =>
    'b;

  /** [str] is {!Fromat.asprintf}. */

  let str: format4('a, Format.formatter, unit, string) => 'a;

  /** [amy fmt] formats any value with [fmt]. */

  let any: format(unit, Format.formatter, unit) => t('a);

  /** [sp] is {!Format.pp_print_space}. */

  let sp: t('a);

  /** [string] is {!Format.pp_print_string}. */

  let string: t(string);

  /** [list] is {!Format.pp_print_list}. */

  let list: (~sep: t(unit)=?, t('a)) => t(list('a));

  /** [text] is {!Format.pp_print_text}. */

  let text: t(string);

  /** [tty style pp_v] formats [pp_v] with style [style]. */

  let tty: (list(Tty.style), t('a)) => t('a);
};

/** Editor interaction. */

module Editor: {
  /** [find ()] finds an editor command in EDITOR or VISUAL (in
      that order). Does not check it's runnable. */

  let find: unit => result(Cmd.t, string);

  /** [edit_string ~ext s] edits strings [s] in a temporary file with
      extension [ext] with an editor found using [find ()]. The resulting
      string is {!String.trim}ed. */

  let edit_string: (~ext: string, string) => result(string, string);

  /** [edit_file s] edits file [s] with an editor found using {!find}. */

  let edit_file: string => result(unit, string);
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
