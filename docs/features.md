# SLIP Features

SLIce Processing is LISP for golang

SLIP is a mostly Common LISP implementation lacking some features and
including many non standard features. Most notable of the extra
features is the ability to extend LISP with Go code. Also included is
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

SLIP is based on Go slices instead of __cons__ cells. The same list,
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

The reason for using Go slices instead of a cons struct is that using
slices uses less memory, puts less pressure on the garbage collector,
and performs better for most operations.

### Missing Features

Common LISP compiles to byte code. This is not part of the SLIP model
which instead compiles to Go types and functions instead. There is no
capacity in SLIP to store the compiled Go types and no plans for such
a feature in the future. SLIP does not support any functions that
involve compilation to byte code.

The exception or error handling in SLIP differs from Common
LISP. Similar features exist in SLIP but those features are different
than those found in Common LISP. SLIP make use of Go `panic` and
`recover` as a model for exception and error handling. The Common LISP
error types are supported and are what is most often recovered when a
panic is raised.

In Go strings are immutable which is reflected in SLIP so the
_nstring_ functions so not modify the original string but instead are
the same as the _string_ versions of the functions.

### Added Features

While Common LISP is the modern version of LISP it is lacking in
support for unicode and threads. SLIP adds that support in the Go
Integration (GI) package. This package includes channels, routines
(threads), panic, recover, a simplier time type, a logger, and a other
functions optimized for use with slices.

Support for plug-ins written in Go is included in SLIP. This makes use
of the Go plugin package and the LISP _require_ function to load those
plug-ins.

## Read and Eval

There are two steps in loading LISP code in SLIP. The code is first
read which builds a set of go types. During reading top level
definition functions such as _defun_, _defvar_, _defmacro_, and
_defparamter_ are evaluated. The second step evaluate the remaining
top level code elements to for Go objects which is the SLIP the
compiled code.

## REPL

A feature that separates LISP from most other languages is the Read
Eval Print Loop or REPL. A REPL provides an environment for
prototyping, testing, and exploring. The SLIP REPL includes Emacs
style key bindings and makes use of ANSI codes to provide help and tab
completion support. History and global settings are "remembered" from
one session to the next.

Another helpful feature is the "stash" feature that lets the user
stash a function into a stash file. An example of this use is when
prototyping functions since the function can be stashed for later use.

## Object Systems

In the early days of LISP the object system was Flavors. Flavors was
the most influential model in the Common LISP Object System (CLOS)
specification. SLIP includes both a Flavors and a CLOS
implementation. CLOS and Flavors are separate in SLIP but share some
functions and functionality.

The principle difference between CLOS and Flavors is the perspective
each has on methods of functions. CLOS is focussed on functions first
and determines when version of a function to use based on the argument
types in the function call. This allows behavior to be associated with
more than one class. As an example if a generic method `quux` is
defined with two arguments then a call with classes `(fixnum
double-float)` can behave differently than `(fixnum fixnum)`.

One of the disadvantages of CLOS in a large system that pulls in code
from external source is name collision. If more than one package
defines `quux` then care must be taken to always use the package
prefix. This restriction encompasses regular, non-generic functions as
well. As an example a CLOS generic method can not be called `first`
since that conflicts with the regular `first` function.

Flavors is class focussed. Methods are associated with a class. This
encapsulates a method's behavior in a class inheritance tree so there
is no bleeding of method names globally. The Flavors implementation is
SLIP is somewhat more efficient than the CLOS implementation in part
dues to the more direct lookup of methods.

## Watch

Sometimes it is helpful to monitor or watch a variable. The watch
package provides a means to not only monitor but to do that remotely
as well.

## Plugins

LISP code can be imported with the `require` function. The `require`
function can also import Go packages with the Go import package. This
allows Go code to be pulled into a running SLIP application.

## JSON

SLIP includes support for arbitrary that represents JSON data. Support
for JSON is provided by [OjG](https://github.com/ohler55/ojg). The
**bag-flavor** represent this JSON data. The data itself is only
accessible through instances of the **bag-flavor**.
