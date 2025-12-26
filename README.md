# [![{}j](assets/slip.svg)](https://github.com/ohler55/slip)

[![Build Status](https://github.com/ohler55/slip/actions/workflows/CI.yml/badge.svg)](https://github.com/ohler55/slip/actions)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ohler55/slip/CI?label=build)
[![Go Report Card](https://goreportcard.com/badge/github.com/ohler55/slip)](https://goreportcard.com/report/github.com/ohler55/slip)
![](assets/coverage-badge.svg)

SLIce Processing is LISP for golang

SLIP is a mostly Common LISP implementation lacking some features and
including many non standard features. Most notable of the extra
features is the ability to extend LISP with Go code. Also included is
a Read Eval Print Loop (REPL) that provides an environment for
prototyping, testing, and exploring SLIP. While not a full
implemenation of Common LISP, SLIP continues to move in that
direction.

A more detailed explanation of some of the features are on the
[features.md page](features.md).

## Using

To use, first install then start the REPL with the **slip**
application. A good place to start is to press `ctrl-h` for help. Then
try out some LISP code. Two of the most help functions are `apropos`
and `describe`. Press `M-?` or `M-/` after a function name to get
pop-up documentation.

```lisp
▶ (apropos 'json)
bag:discover-json (built-in)
bag:json-parse (built-in)
```

```lisp
▶ (describe 'car)
common-lisp:car
  [symbol]

car names a built-in:
  Lambda-List: (arg)
  Return: object
  Documentation:
    car returns the car if arg is a cons, the first element if arg is a list, and nil if arg is nil or an empty list.
  Arguments:
    arg: [list|cons]
      The value to take the first element of.

  Examples:
    (car nil) => nil
    (car '(a . b) => a
    (car '(a b c)) => a
```

The REPL includes many additional features such as muliple line
editing, history, tab completion, and more.

## Installation

Install using `go get`:

```
go get github.com/ohler55/slip/cmd/slip

```

Install using **brew**:

```
brew install slip

```

## Releases

See [CHANGELOG.md](CHANGELOG.md)

## Links

 - [ANSI Common Lisp](http://www.x3.org/tc_home/tc_sd4/x3j13sd4.html)
 - Common LISP reference [LispWorks Common List HyperSpec](tbd)
 - [Flavors](https://franz.com/support/documentation/10.1/doc/flavors.htm)
 - An introduction to CLOS [LISP Cookbook](https://lispcookbook.github.io/cl-cookbook/clos.html)
 - A CLOS guide: [CLOS Guide](https://www.algo.be/cl/documents/clos-guide.html)
 - JSON support provided by [OjG](https://github.com/ohler55/ojg).

## Contributing

Bug fix contributions are welcome. Please create a PR for any
fixes. New features should be discussed first as the SLIP core
generally follows the Common LISP specification as much as
possible. SLIP can be extended with plugins and new features are often
best offered with a separate plugin project. If you would like me to
review a plugin just ask and I'll do my best to provide constructive
comments.

On the subject of using AI, please realize that AI is just a tool and
the final responsibility is you as a developer. Code that is clearly
written by AI with developer cleanup or has the hallmarks of sloppy
code will be rejected early.
