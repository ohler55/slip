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
_tail_ structure used when a __cons__ is expected such as in an
association list.

```
[1][tail{value:2}]
```

The reason for using go slices instead of a cons struct is that using
slices uses less memory, puts less pressure on the garbage collector,
and performs better for most operations.

### Missing Features

TBD

- CLOS
- no byte code files


### Added Features

TBD

- require to load using go plugins
- gi package with channels, threads as go routines, etc
- unicode
- flavors instead of CLOS


## Read and Eval

TBD

## REPL

TBD
