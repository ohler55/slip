# SLIP

SLIce Processing is LISP for golang

SLIP is a Common LISP implementation lacking some features and
including many non standard features. Most notable of the extra
features is the ability to extend LISP with go code. Also included is
a Read Eval Print Loop (REPL) that provides an environment for
prototyping, testing, and exploring SLIP.

## Almost Common LISP

While not a full implemenation of Common LISP, SLIP continues to move
in that direction.

### Cons versus Slice

Most LISP implementations are built on a structure called a __cons__
which is composed of a __car__ and a __cdr__. Lists are composed of a
linked list of __cons__ cells. A list of `(1 2 3)` is composed of
three __cons__ cells.

```
[car|cdr]
  ▼   ▼
  1 [car|cdr]
      ▼   ▼
      2 [car|cdr]
          ▼   ▼
          3  nil
```

SLIP is based on go slices instead of __cons__ cells. The same list,
`(1 2 3)` is:

```
[1][2][3]
```

A __cons__ cell in SLIP is a list of two elements with a special
_tail_ structure used when a __cons__ `(1 . 2)` is expected such as in
an association list.

```
[1][tail{value:2}]
```

The reason for using go slices instead of a cons struct is that using
slices uses less memory, puts less pressure on the garbage collector,
and performs better for most operations.

### Missing Features

Common LISP compiles to byte codes this is not part of the SLIP model
which instead compiles to go types. There is no capacity in SLIP to
store the compiled go types and no plans for such a feature in the
future. Any functions that involve compilation to byte codes are not
support in SLIP.

In the early days of LISP the object system was Flavors. Flavors
served as most influential model when the Common LISP Object System
(CLOS) was defined. SLIP supports a full flavors implementation but at
least initially not CLOS. The reasons for this decision are partly due
to the auther's preference and familiarity with Flavors but also in
part because the Flavors model is clearly separate from regular LISP
functions calls and hence avoids the restrictions and confusion of the
more complex CLOS model and it's overlap with normal function calls.

The exception or error handling in SLIP differs from Common
LISP. Similar features exist in SLIP but they are different as they
integrate better with the underlying go components.

### Added Features

While Common LISP is the modern version of LISP it is lacking in
support for unicode and threads. SLIP adds that support in the Go
Integration (GI) package. This package includes channels, routines
(threads), panic, recover, a simplier time type, a logger, and a few
other functions optimized for use with slices.

Support for plug-ins written in go is included in SLIP. This make use
of the go plugin package and the _require_ function to load those
plug-ins.

## Read and Eval

There are two steps in loading LISP code in SLIP. The code is first
read which builds a set of go types. During the reading top level
definition functions such as _defun_, _defvar_, _defmacro_, and
_defparamter_ are evaluated. The second step evaluate the remaining
top level code elements.

## REPL

A LISP feature that separates LISP from most other languages is the
Read Eval Print Loop or REPL. A REPL provides an environment for
prototyping, testing, and exploring. The SLIP REPL includes Emacs
style key bindings and make use of ANSI codes to provide help and tab
completion support. History and global settings are "remembered" from
one session to the next.
